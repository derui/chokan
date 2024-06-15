use std::{
    fs::{self, File},
    future,
    io::Read,
    net::SocketAddr,
    path::{Path, PathBuf},
    sync::{
        mpsc::{Receiver, Sender},
        Arc, Mutex,
    },
    thread::sleep,
    time::{self, Duration},
};

use chokan_dic::ChokanDictionary;
use clap::Parser;
use dic::base::{dictionary::Dictionary, entry::Entry, word::Word};
use jsonrpsee::{
    server::{RpcServiceBuilder, Server},
    RpcModule,
};
use kkc::frequency::ConversionFrequency;
use postcard::from_bytes;

use session::SessionStore;
use tokio::task::JoinHandle;
use tracing_subscriber::util::SubscriberInitExt;
use user_pref::UserPref;

mod method;
mod method_context;
mod session;
mod user_pref;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// 起動するport。デフォルトは8876
    #[arg(short, long, default_value_t = 8876)]
    port: u16,

    /// 利用する辞書のパス
    #[arg(short, long)]
    dictionary_path: String,

    /// ユーザー辞書を保存するdirectory。
    /// 指定しない場合は、頻度とユーザー辞書は保存されない。
    #[arg(short, long)]
    user_dictionary_dir: Option<String>,

    /// 保存処理を行う間隔を指定する。
    ///
    /// デフォルトは60 = 1分
    #[arg(short, long, default_value_t = 60)]
    seconds_per_save: u8,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let filter = tracing_subscriber::filter::EnvFilter::from_default_env()
        .add_directive("jsonrpsee[method_call{name = \"say_hello\"}]=trace".parse()?);
    tracing_subscriber::FmtSubscriber::builder()
        .with_env_filter(filter)
        .finish()
        .try_init()?;

    let args = Args::parse();
    let mut dictionary = read_dictionary_from(&args.dictionary_path)?;
    let user_dictionary_dir = args
        .user_dictionary_dir
        .clone()
        .map(|v| PathBuf::from(v).into_boxed_path());
    let user_pref = user_dictionary_dir
        .and_then(|v| match UserPref::restore_user_pref(v) {
            Ok(v) => Some(v),
            Err(_) => None,
        })
        .unwrap_or(UserPref::new(
            ConversionFrequency::new(),
            Dictionary::new(vec![]),
            None,
        ));

    {
        for e in user_pref.user_dictionary().entries_ref().iter() {
            let words: Vec<Word> = e.into();
            for word in words {
                let reading = word.reading.iter().collect::<String>();
                let _wording = word.word.iter().collect::<String>();
                let _ = dictionary.graph.standard_trie.insert(&reading);
                dictionary
                    .graph
                    .standard_dic
                    .entry(reading)
                    .and_modify(|v| v.push(word.clone()))
                    .or_insert(vec![word]);
            }
        }
        tracing::info!(
            "Merged user dictionary {} words",
            user_pref.user_dictionary().entries().len()
        );
    }

    let module = define_module(dictionary, user_pref, &args)?;

    run_server(args.port, module).await?;

    let () = future::pending().await;

    Ok(())
}

/// 定期的にユーザー辞書を保存する処理をspawnする
fn spawn_save_user_pref_per_count(pref: Arc<Mutex<UserPref>>, tx: Receiver<()>) -> JoinHandle<()> {
    tokio::spawn(async move {
        loop {
            if let Ok(()) = tx.recv() {
                let pref = pref.lock().unwrap();
                match pref.save_user_dictionary() {
                    Ok(()) => {
                        tracing::debug!("User dictionary saved");
                    }
                    Err(e) => {
                        tracing::error!("Failed to save user dictionary: {}", e);
                    }
                }
            }
        }
    })
}

/// 時間ベースでユーザー辞書を保存する処理をspawnする
fn spawn_periodic_user_pref_save(seconds_per_save: u8, tx: Sender<()>) -> JoinHandle<()> {
    tokio::spawn(async move {
        let mut current = time::SystemTime::now();

        loop {
            if let Ok(elapsed) = current.elapsed() {
                if elapsed.as_secs() >= seconds_per_save as u64 {
                    current = time::SystemTime::now();
                    tx.send(()).expect("should success");
                }
            }

            sleep(Duration::from_secs(1));
        }
    })
}

/// ユーザー辞書に更新があった際に全体辞書を更新するtaskをspawnする
///
/// # Arguments
/// * `dictionary` - 利用する辞書
/// * `entry_receiver` - 更新されたエントリを受信するためのchannel
fn spawn_update_dictionary_with_entry(
    dict: Arc<Mutex<ChokanDictionary>>,
    user_pref: Arc<Mutex<UserPref>>,
    tx: Receiver<Entry>,
) -> JoinHandle<()> {
    tokio::spawn(async move {
        loop {
            if let Ok(entry) = tx.recv() {
                {
                    let mut user_pref = user_pref.lock().unwrap();
                    user_pref.user_dictionary_mut().add_entry(entry.clone());
                }

                let mut dict = dict.lock().unwrap();
                let words: Vec<Word> = entry.into();

                for word in words {
                    let reading = word.reading.iter().collect::<String>();
                    let _wording = word.word.iter().collect::<String>();
                    let _ = dict.graph.standard_trie.insert(&reading);
                    dict.graph
                        .standard_dic
                        .entry(reading)
                        .and_modify(|v| v.push(word.clone()))
                        .or_insert(vec![word]);
                }
            }
        }
    })
}

/**
RPCモジュールを定義する。

ここでは、別途定義したcallbackを実行する形になる。lifecycleの関係上、dictionaryはmoveされる必要がある。

# Arguments
* `dictionary` - 利用する辞書

# Returns
定義したRPCモジュール
*/
fn define_module(
    dictionary: ChokanDictionary,
    user_pref: UserPref,
    args: &Args,
) -> anyhow::Result<RpcModule<method_context::MethodContext>> {
    let dictionary = Arc::new(Mutex::new(dictionary));
    let user_pref = Arc::new(Mutex::new(user_pref));

    let ctx = method_context::MethodContext::new(dictionary.clone(), user_pref.clone());
    let mut module = RpcModule::new(ctx);
    let (session_sender, session_receiver) = std::sync::mpsc::channel();
    let (conversion_notifier, conversion_reciever) = std::sync::mpsc::channel();
    let (entry_sender, entry_reciever) = std::sync::mpsc::channel();
    let store = Arc::new(Mutex::new(SessionStore::new()));

    method::make_get_candidates_method(&mut module, session_sender.clone())?;
    method::make_get_tankan_candidates_method(&mut module)?;
    method::make_update_frequency_method(&mut module, store.clone(), entry_sender.clone())?;
    method::make_register_word(&mut module, entry_sender.clone())?;
    method::make_get_proper_candidates_method(&mut module, session_sender.clone())?;
    method::make_get_alphabetic_candidate_method(&mut module)?;

    let store_in_thread = store.clone();
    // ここでのthreadは、後始末する必要がない
    tokio::spawn(async move {
        loop {
            if let Ok(session) = session_receiver.recv() {
                let (id, candidates, context) = session;
                store_in_thread
                    .lock()
                    .unwrap()
                    .add_session(&id, &candidates, &context);
            }
        }
    });
    spawn_save_user_pref_per_count(user_pref.clone(), conversion_reciever);
    spawn_update_dictionary_with_entry(dictionary.clone(), user_pref.clone(), entry_reciever);
    spawn_periodic_user_pref_save(args.seconds_per_save, conversion_notifier.clone());

    Ok(module)
}

/**
サーバーを起動する。ここで起動したサーバーは、基本的にsighupを受け取るまで停止しない。
 */
async fn run_server(
    port: u16,
    rpc_module: RpcModule<method_context::MethodContext>,
) -> anyhow::Result<()> {
    let addr = format!("127.0.0.1:{}", port).parse::<SocketAddr>()?;
    let rpc_middleware = RpcServiceBuilder::new().rpc_logger(1024);
    let server = Server::builder()
        .set_rpc_middleware(rpc_middleware)
        .build(addr)
        .await?;
    let handle = server.start(rpc_module);

    tracing::info!("Launching server on {}", addr);

    // server自体は基本的に停止しない
    tokio::spawn(handle.stopped());

    Ok(())
}

/**
指定したパスから辞書を読み込む。

辞書の形式は、chokan-dic から生成されたものでなければならない。

# Arguments
* `path` - 辞書のパス

# Returns
読み込んだ辞書
*/
fn read_dictionary_from(path: &String) -> anyhow::Result<ChokanDictionary> {
    let mut input = File::open(fs::canonicalize(Path::new(path))?)?;
    let mut buf = Vec::new();
    input.read_to_end(&mut buf)?;
    let data = from_bytes::<ChokanDictionary>(&buf)?;

    tracing::info!("Loading dictionary from {}...finished", path);

    Ok(data)
}
