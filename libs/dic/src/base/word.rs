use serde::{Deserialize, Serialize};

use super::speech::{AffixVariant, ParticleType, Speech};

// 単語そのものを表現する型
// この型では、例えば「食べる」のような活用形を持つ単語から活用された「食べない」がwordとして設定されている。
// entryは辞書としては利用しやすいのだが、検索などではこの形に展開されている方が都合が良い。
#[derive(Debug, Eq, PartialEq, Serialize, Deserialize, Clone, Hash)]
pub struct Word {
    pub word: Vec<char>,
    pub reading: Vec<char>,
    pub speech: Speech,
}

impl Word {
    // 新しいWordを生成する
    pub fn new(word: &str, reading: &str, speech: Speech) -> Word {
        Word {
            word: word.chars().collect(),
            reading: reading.chars().collect(),
            speech,
        }
    }

    /// 単語同士が、文法上接続可能かどうかを返す
    ///
    /// ここでの接続可能性は、基本的に文節を構成可能かどうか、に主眼が置かれている。
    ///
    /// # Arguments
    /// * `next` - 接続可能かどうかを調べる単語
    ///
    /// # Returns
    /// 接続可能ならtrue、そうでないならfalse
    pub fn can_connect(&self, next: &Self) -> bool {
        match (&self.speech, &next.speech) {
            // 格助詞は体言の後につく
            (Speech::Noun(_), Speech::Particle(ParticleType::Case)) => true,
            // 接続助詞は用言または助動詞の後につく
            (Speech::Verb(_), Speech::Particle(ParticleType::Conjunctive)) => true,
            (Speech::AuxiliaryVerb, Speech::Particle(ParticleType::Conjunctive)) => true,
            // 副助詞は色々つくことができる
            (_, Speech::Particle(ParticleType::Adverbial)) => true,
            // 終助詞は文末につくが、文末を保証するのがむずかしいので、副助詞と同様にする
            (_, Speech::Particle(ParticleType::SentenceFinal)) => true,
            // 助動詞は用言の後につく
            (Speech::Verb(_), Speech::AuxiliaryVerb) => true,
            // 接頭辞は動詞または名詞の前につく
            (Speech::Affix(AffixVariant::Prefix), Speech::Noun(_)) => true,
            (Speech::Affix(AffixVariant::Prefix), Speech::Verb(_)) => true,
            // 接尾辞は動詞または名詞の後につく。ただしかな漢字変換では、基本的に接尾辞は名詞の後につく
            (Speech::Noun(_), Speech::Affix(AffixVariant::Suffix)) => true,
            (Speech::Verb(_), Speech::Affix(AffixVariant::Suffix)) => true,

            // 上記以外は接続しないものとして扱う
            _ => false,
        }
    }
}
