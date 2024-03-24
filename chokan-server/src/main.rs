use std::{
    fs::{self, File},
    future,
    io::Read,
    net::SocketAddr,
    path::Path,
};

use chokan_dic::ChokanDictionary;
use clap::Parser;
use jsonrpsee::{
    server::{RpcServiceBuilder, Server},
    RpcModule,
};
use postcard::from_bytes;

use tracing_subscriber::util::SubscriberInitExt;

mod method;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// 起動するport。デフォルトは8876
    #[arg(short, long, default_value_t = 8876)]
    port: u16,

    /// 利用する辞書のパス
    #[arg(short, long)]
    dictionary_path: String,
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
    let module = define_module(dictionary)?;

    run_server(args.port, module).await?;

    let () = future::pending().await;

    Ok(())
}

/**
RPCモジュールを定義する。

ここでは、別途定義したcallbackを実行する形になる。lifecycleの関係上、dictionaryはmoveされる必要がある。

# Arguments
* `dictionary` - 利用する辞書

# Returns
定義したRPCモジュール
*/
fn define_module(dictionary: ChokanDictionary) -> anyhow::Result<RpcModule<ChokanDictionary>> {
    let mut module = RpcModule::new(dictionary);

    method::make_get_candidates_method(&mut module)?;
    method::make_get_tankan_candidates_method(&mut module)?;

    Ok(module)
}

/**
サーバーを起動する。ここで起動したサーバーは、基本的にsighupを受け取るまで停止しない。
 */
async fn run_server(port: u16, rpc_module: RpcModule<ChokanDictionary>) -> anyhow::Result<()> {
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
