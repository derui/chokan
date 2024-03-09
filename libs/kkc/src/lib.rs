use std::collections::HashMap;

use dic::base::word::Word;
use serde::{Deserialize, Serialize};

/// 解析グラフ上で利用する辞書の形式。
///
/// ここで利用される辞書は、 `chokan-dic` プログラムから生成されるものが利用される
#[derive(Serialize, Deserialize)]
pub struct GraphDictionary {
    pub standard_trie: trie::Trie,
    pub standard_dic: HashMap<String, Vec<Word>>,
    // 付属語
    pub ancillary_trie: trie::Trie,
    pub ancillary_dic: HashMap<String, Vec<Word>>,
}
