use std::collections::HashMap;

use dic::base::{
    speech::{NounVariant, ParticleType, Speech, VerbForm},
    word::Word,
};

use crate::GraphDictionary;

pub const LABELS: [char; 72] = [
    'あ', 'い', 'う', 'え', 'お', 'か', 'き', 'く', 'け', 'こ', 'さ', 'し', 'す', 'せ', 'そ', 'た',
    'ち', 'つ', 'て', 'と', 'な', 'に', 'ぬ', 'ね', 'の', 'は', 'ひ', 'ふ', 'へ', 'ほ', 'ま', 'み',
    'む', 'め', 'も', 'や', 'ゆ', 'よ', 'ら', 'り', 'る', 'れ', 'ろ', 'わ', 'を', 'ん', 'が', 'ぎ',
    'ぐ', 'げ', 'ご', 'ざ', 'じ', 'ず', 'ぜ', 'ぞ', 'だ', 'ぢ', 'づ', 'で', 'ど', 'ば', 'び', 'ぶ',
    'べ', 'ぼ', 'ぱ', 'ぴ', 'ぷ', 'ぺ', 'ぽ', 'っ',
];

/// テスト用の辞書を提供する
pub fn new_dic() -> GraphDictionary {
    let keys = LABELS.to_vec();
    let mut standard_trie = trie::Trie::from_keys(&keys);
    let mut ancillary_trie = trie::Trie::from_keys(&keys);

    standard_trie.insert("くるま").unwrap();
    standard_trie.insert("くる").unwrap();
    ancillary_trie.insert("まで").unwrap();
    ancillary_trie.insert("で").unwrap();

    GraphDictionary {
        standard_trie,
        standard_dic: HashMap::from([
            (
                "くるま".to_string(),
                vec![Word::new("車", "くるま", Speech::Noun(NounVariant::Common))],
            ),
            (
                "くる".to_string(),
                vec![
                    Word::new(
                        "来る",
                        "くる",
                        Speech::Verb(VerbForm::Hen("カ".to_string())),
                    ),
                    Word::new(
                        "繰る",
                        "くる",
                        Speech::Verb(VerbForm::Godan("ラ".to_string())),
                    ),
                ],
            ),
        ]),
        ancillary_trie,
        ancillary_dic: HashMap::from([
            (
                "まで".to_string(),
                vec![Word::new(
                    "まで",
                    "まで",
                    Speech::Particle(ParticleType::Adverbial),
                )],
            ),
            (
                "で".to_string(),
                vec![Word::new("で", "で", Speech::Particle(ParticleType::Case))],
            ),
        ]),
    }
}
