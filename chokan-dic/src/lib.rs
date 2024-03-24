use kkc::{GraphDictionary, TankanDictionary};
use serde::{Deserialize, Serialize};

/**
chokan全体で利用する共通辞書の形式。

ユーザー辞書以外は、この形式で事前に作成される
*/
#[derive(Serialize, Deserialize)]
pub struct ChokanDictionary {
    /**
    解析グラフ用の辞書構成
    */
    pub graph: GraphDictionary,

    /**
    単漢字向けの辞書
    */
    pub tankan: TankanDictionary,
}
