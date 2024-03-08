use serde::{Deserialize, Serialize};

use super::speech::Speech;

// 単語そのものを表現する型
// この型では、例えば「食べる」のような活用形を持つ単語から活用された「食べない」がwordとして設定されている。
// entryは辞書としては利用しやすいのだが、検索などではこの形に展開されている方が都合が良い。
#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Word {
    pub word: String,
    pub reading: String,
    pub speech: Speech,
}

impl Word {
    // 新しいWordを生成する
    pub fn new(word: &str, reading: &str, speech: Speech) -> Word {
        Word {
            word: word.to_string(),
            reading: reading.to_string(),
            speech,
        }
    }
}
