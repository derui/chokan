use std::{collections::HashSet, fmt::Display};

use serde::{Deserialize, Serialize};

// 単語における品詞
#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum Speech {
    Noun(NounVariant),      // 名詞
    Verb(VerbForm),         // 動詞。引数は辞書系で利用する活用
    Adjective,              // 形容詞
    Adverb,                 // 副詞
    AdjectivalVerb,         // 形容動詞
    Verbatim,               // 感動詞
    Conjunction,            // 接続詞
    Particle(ParticleType), // 助詞
    AuxiliaryVerb,          // 助動詞
    PreNounAdjectival,      // 連体詞
    Counter,                // 助数詞
    Affix(AffixVariant),    // 接辞
}

impl Speech {
    /// 各品詞の活用形の一覧を返す
    ///
    /// # Arguments
    ///
    /// - `stem_reading` - 語幹の読み
    ///
    /// 活用が存在しない品詞の場合は、empty stringのみを含むvecを返す
    pub fn to_forms(&self, stem_reading: &str) -> HashSet<String> {
        match self {
            Speech::Noun(_) => ["".to_string()].iter().cloned().collect(),
            Speech::Verb(form) => form.to_forms(stem_reading),
            Speech::Adjective => ["い", "く", "け", "かっ", "う"]
                .iter()
                .map(|v| v.to_string())
                .collect(),
            Speech::Adverb => ["".to_string()].iter().cloned().collect(),
            Speech::AdjectivalVerb => ["だ", "だっ", "な", "なら", "で", "に"]
                .iter()
                .map(|v| v.to_string())
                .collect(),
            Speech::Verbatim => ["".to_string()].iter().cloned().collect(),
            Speech::Conjunction => ["".to_string()].iter().cloned().collect(),
            Speech::Particle(_) => ["".to_string()].iter().cloned().collect(),
            Speech::AuxiliaryVerb => ["".to_string()].iter().cloned().collect(),
            Speech::PreNounAdjectival => ["".to_string()].iter().cloned().collect(),
            Speech::Counter => ["".to_string()].iter().cloned().collect(),
            Speech::Affix(_) => ["".to_string()].iter().cloned().collect(),
        }
    }
}

impl Display for Speech {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Speech::Noun(v) => write!(f, "{}名詞", v),
            Speech::Verb(form) => write!(f, "{}", form),
            Speech::Adjective => write!(f, "形容詞"),
            Speech::Adverb => write!(f, "副詞"),
            Speech::AdjectivalVerb => write!(f, "形容動詞"),
            Speech::Verbatim => write!(f, "感動詞"),
            Speech::Conjunction => write!(f, "接続詞"),
            Speech::Particle(typ) => write!(f, "{}助詞", typ),
            Speech::AuxiliaryVerb => write!(f, "助動詞"),
            Speech::PreNounAdjectival => write!(f, "連体詞"),
            Speech::Counter => write!(f, "助数詞"),
            Speech::Affix(v) => write!(f, "{}辞", v),
        }
    }
}

/// 接辞の種類
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum AffixVariant {
    Prefix, // 接頭辞
    Suffix, // 接尾辞
}

impl Display for AffixVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AffixVariant::Prefix => write!(f, "接頭"),
            AffixVariant::Suffix => write!(f, "接尾"),
        }
    }
}

/// 名詞の種類
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum NounVariant {
    Sahen,  // サ変名詞
    Proper, // 固有名詞
    Common, // 一般名詞
}

impl Display for NounVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NounVariant::Sahen => write!(f, "サ変"),
            NounVariant::Proper => write!(f, "固有"),
            NounVariant::Common => write!(f, "一般"),
        }
    }
}

/// 助詞の種類
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum ParticleType {
    Case,          // 格助詞
    Adverbial,     // 副助詞
    Conjunctive,   // 接続助詞
    SentenceFinal, // 終助詞
    Other,         // その他
}

impl Display for ParticleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParticleType::Case => write!(f, "格"),
            ParticleType::Adverbial => write!(f, "副"),
            ParticleType::Conjunctive => write!(f, "接続"),
            ParticleType::SentenceFinal => write!(f, "終"),
            ParticleType::Other => write!(f, ""),
        }
    }
}

/// 動詞の活用形
/// 引数は、各活用で利用する段である
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum VerbForm {
    Godan(String),       // 五段活用
    Yodan(String),       // 四段活用
    SimoIchidan(String), // 下一段活用
    KamiIchidan(String), // 上一段活用
    SimoNidan(String),   // 下二段活用
    KamiNidan(String),   // 上二段活用
    Hen(String),         // 変格活用
}

impl VerbForm {
    /// 各動詞の活用形の一覧を返す
    ///
    /// # Arguments
    ///
    /// - `stem_reading` - 語幹の読み
    ///
    /// 活用形は、語幹の読みによって変化するケースがあるため、それらを含めて対応をする。
    pub fn to_forms(&self, stem_reading: &str) -> HashSet<String> {
        let vec = match self {
            VerbForm::Godan(row) => match row.as_str() {
                "カ" => match stem_reading
                    .chars()
                    .last()
                    .map(|v| v.to_string())
                    .unwrap_or_default()
                    .as_str()
                {
                    "い" => vec!["く", "か", "こ", "き", "け", "っ"],
                    _ => vec!["く", "か", "こ", "き", "け", "い"],
                },
                "ガ" => vec!["ぐ", "が", "ご", "ぎ", "げ", "い"],
                "サ" => vec!["す", "さ", "そ", "し", "せ"],
                "ザ" => vec!["ず", "ざ", "ぞ", "じ", "ぜ"],
                "タ" => vec!["つ", "た", "と", "ち", "て", "っ"],
                "ナ" => vec!["ぬ", "な", "の", "に", "ね", "ん"],
                "バ" => vec!["ぶ", "ば", "ぼ", "び", "べ", "ん"],
                "マ" => vec!["む", "ま", "も", "み", "め", "ん"],
                "ラ" => vec!["る", "ら", "ろ", "り", "れ", "っ"],
                "ワ" => vec!["う", "わ", "い", "え", "お", "っ"],
                _ => panic!("Can not get okuri for godan verb with {}", row),
            },

            VerbForm::Yodan(row) => match row.as_str() {
                "カ" => vec!["く", "か", "き", "け"],
                "ガ" => vec!["ぐ", "が", "ぎ", "げ"],
                "サ" => vec!["す", "さ", "し", "せ"],
                "タ" => vec!["つ", "た", "ち", "て"],
                "ダ" => vec!["づ", "だ", "ぢ", "で"],
                "ナ" => vec!["ぬ", "な", "に", "ね"],
                "ハ" => vec!["ふ", "は", "ひ", "へ"],
                "バ" => vec!["ぶ", "ば", "び", "べ"],
                "マ" => vec!["む", "ま", "み", "め"],
                "ラ" => vec!["る", "ら", "り", "れ"],
                _ => panic!("Can not get okuri for yodan verb with {}", row),
            },
            VerbForm::SimoIchidan(row) => match row.as_str() {
                "ア" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["え"]
                    }
                }
                "カ" => vec!["け"],
                "ガ" => vec!["げ"],
                "サ" => vec!["せ"],
                "ザ" => vec!["ぜ"],
                "タ" => vec!["て"],
                "ダ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["で"]
                    }
                }
                "ナ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["ね"]
                    }
                }
                "ハ" => vec![""],
                "バ" => vec!["べ"],
                "マ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["め"]
                    }
                }
                "ラ" => vec!["れ"],
                _ => panic!("Can not get okuri for shimoichidan verb with {}", row),
            },
            VerbForm::KamiIchidan(row) => match row.as_str() {
                "ア" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["い"]
                    }
                }
                "カ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["き"]
                    }
                }
                "ガ" => vec!["ぎ"],
                "ザ" => vec!["じ"],
                "タ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["ち"]
                    }
                }
                "ナ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["に"]
                    }
                }
                "ハ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["ひ"]
                    }
                }
                "バ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["び"]
                    }
                }
                "マ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["み"]
                    }
                }
                "ラ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["り"]
                    }
                }
                "ワ" => {
                    if stem_reading.len() == 1 {
                        vec![""]
                    } else {
                        vec!["ゐ"]
                    }
                }
                _ => panic!("Can not get okuri for kamiichidan verb with {}", row),
            },
            VerbForm::SimoNidan(row) => match row.as_str() {
                // ア行下二は、「得る」のみ
                "ア" => vec!["え", "う"],
                "カ" => vec!["け", "く"],
                "ガ" => vec!["げ", "ぐ"],
                "サ" => vec!["せ", "す"],
                "ザ" => vec!["ぜ", "ず"],
                "タ" => vec!["て", "つ"],
                "ダ" => vec!["で", "づ"],
                "ナ" => vec!["ぬ", "ね"],
                "ハ" => vec!["へ", "ふ"],
                "バ" => vec!["べ", "ぶ"],
                "マ" => vec!["め", "む"],
                "ラ" => vec!["れ", "る"],
                "ヤ" => vec!["え", "ゆ"],
                "ワ" => vec!["ゑ", "う"],
                _ => panic!("Can not get okuri for shimonidan verb with {}", row),
            },
            VerbForm::KamiNidan(row) => match row.as_str() {
                "カ" => vec!["き", "く"],
                "ガ" => vec!["ぎ", "ぐ"],
                "タ" => vec!["ち", "つ"],
                "ダ" => vec!["ぢ", "づ"],
                "ハ" => vec!["ひ", "ふ"],
                "バ" => vec!["び", "ぶ"],
                "マ" => vec!["み", "む"],
                "ヤ" => vec!["い", "ゆ"],
                "ラ" => vec!["り", "る"],
                _ => panic!("Can not get okuri for kaminidan verb with {}", row),
            },
            VerbForm::Hen(row) => match row.as_str() {
                // カ行変格活用では、基本的に語幹自体が無いという考え方がある。
                "カ" => vec!["こ", "き", "く"],
                "サ" => vec!["さ", "せ", "し", "す"],
                "ラ" => vec!["ら", "れ", "り", "る"],
                "ナ" => vec!["な", "ね", "に", "ぬ"],
                _ => panic!("Can not get okuri for henkaku verb with {}", row),
            },
        };
        vec.iter().map(|v| v.to_string()).collect()
    }
}

impl Display for VerbForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerbForm::Godan(row) => write!(f, "{}行五段", row),
            VerbForm::Yodan(row) => write!(f, "{}行四段", row),
            VerbForm::SimoIchidan(row) => write!(f, "{}行下一", row),
            VerbForm::KamiIchidan(row) => write!(f, "{}行上一", row),
            VerbForm::SimoNidan(row) => write!(f, "{}行下二", row),
            VerbForm::KamiNidan(row) => write!(f, "{}行上二", row),
            VerbForm::Hen(row) => write!(f, "{}行変", row),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_verb_form() {
        let form = VerbForm::Godan("カ".to_string());
        assert_eq!(form.to_string(), "カ行五段");
    }

    #[test]
    fn test_display_affix() {
        assert_eq!(Speech::Affix(AffixVariant::Prefix).to_string(), "接頭辞");
        assert_eq!(Speech::Affix(AffixVariant::Suffix).to_string(), "接尾辞");
    }
}
