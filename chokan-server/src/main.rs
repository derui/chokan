use std::{
    fs::{self, File},
    future::{self},
    io::Read,
    net::SocketAddr,
    path::{Path, PathBuf},
    sync::{mpsc::Receiver, Arc, Mutex},
};

use chokan_dic::ChokanDictionary;
use clap::Parser;
use dic::base::dictionary::Dictionary;
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

    /// 保存するまでの変換回数のタイミングを指定する。
    ///
    /// デフォルトは100
    #[arg(short, long, default_value_t = 100)]
    number_of_conversions: u8,
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
    let dictionary = read_dictionary_from(&args.dictionary_path)?;
    let user_dictionary_dir = args
        .user_dictionary_dir
        .map(|v| PathBuf::from(v).into_boxed_path());
    let module = define_module(dictionary, user_dictionary_dir, args.number_of_conversions)?;

    run_server(args.port, module).await?;

    let () = future::pending().await;

    Ok(())
}

/// 定期的にユーザー辞書を保存する処理をspawnする
fn spawn_save_user_pref_per_count(
    pref: Arc<Mutex<UserPref>>,
    number_of_conversion: u8,
    tx: Receiver<()>,
) -> JoinHandle<()> {
    tokio::spawn(async move {
        let mut conversion_counter: u32 = 0;
        loop {
            if let Ok(()) = tx.recv() {
                if conversion_counter + 1 >= number_of_conversion as u32 {
                    conversion_counter = 0;
                    let pref = pref.lock().unwrap();
                    match pref.save_user_dictionary() {
                        Ok(()) => {
                            tracing::debug!("User dictionary saved");
                        }
                        Err(e) => {
                            tracing::error!("Failed to save user dictionary: {}", e);
                        }
                    }
                } else {
                    conversion_counter += 1;
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
    path: Option<Box<Path>>,
    number_of_conversion: u8,
) -> anyhow::Result<RpcModule<method_context::MethodContext>> {
    let dictionary = Arc::new(Mutex::new(dictionary));
    let user_pref = Arc::new(Mutex::new(user_pref::UserPref::new(
        ConversionFrequency::new(),
        Dictionary::new(vec![]),
        path,
    )));

    let ctx = method_context::MethodContext::new(dictionary, user_pref.clone());
    let mut module = RpcModule::new(ctx);
    let (session_sender, session_receiver) = std::sync::mpsc::channel();
    let (conversion_notifier, conversion_reciever) = std::sync::mpsc::channel();
    let store = Arc::new(Mutex::new(SessionStore::new()));

    method::make_get_candidates_method(&mut module, session_sender)?;
    method::make_get_tankan_candidates_method(&mut module)?;
    method::make_update_frequency_method(&mut module, store.clone(), conversion_notifier)?;

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
    spawn_save_user_pref_per_count(user_pref.clone(), number_of_conversion, conversion_reciever);

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
