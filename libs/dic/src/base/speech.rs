use std::{collections::HashSet, fmt::Display};

use serde::{Deserialize, Serialize};

// 単語における品詞
#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize, Hash)]
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
    /// 接辞。接頭辞や接尾辞を含む
    Affix(AffixVariant), // 接辞
}

impl Speech {
    /// 単語の文字列から品詞を推測する
    ///
    /// ここでの推測は、主に動詞の活用を判定するものであり、かつ完全ではない。判定は以下の順に行う。
    /// * 末尾が動詞の否定形である - **動詞**
    /// * 末尾が形容詞の基本形である - **形容詞**
    /// * 末尾が形容動詞の基本形である - **形容動詞**
    /// * 上記以外の場合は、**一般名詞**として判定する
    ///
    /// # Arguments
    /// * `word` - 送り仮名を含む単語
    ///
    /// # Returns
    /// 推測された品詞と語幹
    pub(crate) fn guess(word: &str) -> (Speech, String) {
        let word = word.chars().collect::<Vec<_>>();

        if word.ends_with(&['な', 'い']) && word.len() >= 3 {
            let char = word[word.len() - 3];
            if let Some(form) = VerbForm::guess_form(char) {
                return (
                    Speech::Verb(form),
                    word[0..(word.len() - 3)].iter().collect(),
                );
            }
        }
        if word.ends_with(&['い']) {
            return (
                Speech::Adjective,
                word[0..(word.len() - 1)].iter().collect(),
            );
        }
        if word.ends_with(&['だ']) {
            return (
                Speech::AdjectivalVerb,
                word[0..(word.len() - 1)].iter().collect(),
            );
        }

        (Speech::Noun(NounVariant::Common), word.iter().collect())
    }

    /// 各品詞の活用形の一覧を返す
    ///
    /// # Arguments
    ///
    /// - `stem_reading` - 語幹の読み
    ///
    /// 活用が存在しない品詞の場合は、empty stringのみを含むvecを返す
    /// # Returns
    /// `(stem, stem_reading)` となるような活用形の一覧
    pub fn to_forms(&self, stem: &str, stem_reading: &str) -> HashSet<(String, String)> {
        match self {
            Speech::Noun(_) => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
            Speech::Verb(form) => form.to_forms(stem, stem_reading),
            Speech::Adjective => ["い", "く", "け", "かっ", "う"]
                .iter()
                .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
                .collect(),
            Speech::Adverb => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
            Speech::AdjectivalVerb => ["だ", "だっ", "な", "なら", "で", "に"]
                .iter()
                .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
                .collect(),
            Speech::Verbatim => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
            Speech::Conjunction => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
            Speech::Particle(_) => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
            Speech::AuxiliaryVerb => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
            Speech::PreNounAdjectival => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
            Speech::Counter => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
            Speech::Affix(_) => [(stem.to_string(), stem_reading.to_string())]
                .iter()
                .cloned()
                .collect(),
        }
    }

    /// 該当の品詞が、付属語かどうかを返す
    pub fn is_ancillary(&self) -> bool {
        match self {
            Speech::Noun(_) => false,
            Speech::Verb(_) => false,
            Speech::Adjective => false,
            Speech::Adverb => false,
            Speech::AdjectivalVerb => false,
            Speech::Verbatim => false,
            Speech::Conjunction => false,
            Speech::Particle(_) => true,
            Speech::AuxiliaryVerb => true,
            Speech::PreNounAdjectival => false,
            Speech::Counter => false,
            Speech::Affix(_) => true,
        }
    }

    /// 該当の品詞が、接辞かどうかを返す
    pub fn is_affix(&self) -> bool {
        match self {
            Speech::Affix(_) => true,
            _ => false,
        }
    }

    /// 該当の品詞が、接頭辞かどうかを返す
    pub fn is_prefix(&self) -> bool {
        match self {
            Speech::Affix(AffixVariant::Prefix) => true,
            _ => false,
        }
    }

    /// 該当の品詞が、接尾辞かどうかを返す
    pub fn is_suffix(&self) -> bool {
        match self {
            Speech::Affix(AffixVariant::Suffix) => true,
            _ => false,
        }
    }

    /// 該当の品詞が、固有名詞かどうかを返す
    pub fn is_noun_proper(&self) -> bool {
        match self {
            Speech::Noun(NounVariant::Proper) => true,
            _ => false,
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
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Hash)]
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
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Hash)]
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
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Hash)]
pub enum ParticleType {
    /// 格助詞
    Case,
    /// 副助詞
    Adverbial,
    /// 接続助詞
    Conjunctive,
    /// 終助詞
    SentenceFinal,
    /// その他
    Other,
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
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Hash)]
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
    ///
    /// # Returns
    /// `(stem, stem_reading)` となるような活用形の一覧。stem/stem_readingはそれぞれ語幹に活用形を含めたものである
    pub fn to_forms(&self, stem: &str, stem_reading: &str) -> HashSet<(String, String)> {
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
            }
            .iter()
            .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
            .collect::<Vec<_>>(),

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
            }
            .iter()
            .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
            .collect::<Vec<_>>(),
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
            }
            .iter()
            .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
            .collect::<Vec<_>>(),
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
            }
            .iter()
            .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
            .collect::<Vec<_>>(),
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
            }
            .iter()
            .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
            .collect::<Vec<_>>(),
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
            }
            .iter()
            .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
            .collect::<Vec<_>>(),
            VerbForm::Hen(row) => match row.as_str() {
                // カ行変格活用では、基本的に語幹自体が無いという考え方がある。
                "カ" => ["こ", "き", "くる", "こい", "くれ"]
                    .iter()
                    .cloned()
                    .map(|v| {
                        let reading_last = stem_reading.char_indices().last().unwrap().0;
                        (
                            format!("{}{}", stem, v.chars().skip(1).collect::<String>()),
                            format!("{}{}", &stem_reading[0..reading_last], v),
                        )
                    })
                    .collect::<Vec<_>>(),
                "サ" => ["さ", "せ", "し", "す"]
                    .iter()
                    .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
                    .collect::<Vec<_>>(),
                "ラ" => ["ら", "れ", "り", "る"]
                    .iter()
                    .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
                    .collect::<Vec<_>>(),
                "ナ" => ["な", "ね", "に", "ぬ"]
                    .iter()
                    .map(|v| (format!("{}{}", stem, v), format!("{}{}", stem_reading, v)))
                    .collect::<Vec<_>>(),
                _ => panic!("Can not get okuri for henkaku verb with {}", row),
            },
        };
        vec.iter().cloned().collect()
    }

    /// 文字に対応する活用形を推測する
    ///
    /// # Arguments
    ///
    /// * `ch` - 判定用の文字
    ///
    /// # Returns
    /// 対応する活用形。存在しない場合はNone
    pub fn guess_form(ch: char) -> Option<VerbForm> {
        match ch {
            // 五段活用
            'か' | 'こ' => Some(VerbForm::Godan("カ".to_string())),
            'が' | 'ご' => Some(VerbForm::Godan("ガ".to_string())),
            'さ' | 'そ' => Some(VerbForm::Godan("サ".to_string())),
            'た' | 'と' => Some(VerbForm::Godan("タ".to_string())),
            'な' | 'の' => Some(VerbForm::Godan("ナ".to_string())),
            'ば' | 'ぼ' => Some(VerbForm::Godan("バ".to_string())),
            'ま' | 'も' => Some(VerbForm::Godan("マ".to_string())),
            'ら' | 'ろ' => Some(VerbForm::Godan("ラ".to_string())),
            'わ' | 'お' => Some(VerbForm::Godan("ワ".to_string())),
            // 上一段活用
            'い' => Some(VerbForm::KamiIchidan("ア".to_string())),
            'き' => Some(VerbForm::KamiIchidan("カ".to_string())),
            'ぎ' => Some(VerbForm::KamiIchidan("ガ".to_string())),
            'し' => Some(VerbForm::KamiIchidan("サ".to_string())),
            'じ' => Some(VerbForm::KamiIchidan("ザ".to_string())),
            'ち' => Some(VerbForm::KamiIchidan("タ".to_string())),
            'に' => Some(VerbForm::KamiIchidan("ナ".to_string())),
            'び' => Some(VerbForm::KamiIchidan("バ".to_string())),
            'み' => Some(VerbForm::KamiIchidan("マ".to_string())),
            'り' => Some(VerbForm::KamiIchidan("ラ".to_string())),
            // 下一段活用
            'え' => Some(VerbForm::SimoIchidan("ア".to_string())),
            'け' => Some(VerbForm::SimoIchidan("カ".to_string())),
            'げ' => Some(VerbForm::SimoIchidan("ガ".to_string())),
            'せ' => Some(VerbForm::SimoIchidan("サ".to_string())),
            'ぜ' => Some(VerbForm::SimoIchidan("ザ".to_string())),
            'て' => Some(VerbForm::SimoIchidan("タ".to_string())),
            'で' => Some(VerbForm::SimoIchidan("ダ".to_string())),
            'ね' => Some(VerbForm::SimoIchidan("ナ".to_string())),
            'べ' => Some(VerbForm::SimoIchidan("バ".to_string())),
            'め' => Some(VerbForm::SimoIchidan("マ".to_string())),
            'れ' => Some(VerbForm::SimoIchidan("ラ".to_string())),
            _ => None,
        }
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

    #[test]
    fn test_ancillary_check() {
        // arrange

        // act

        // assert
        // 一通りの品詞をcheckする
        assert!(!Speech::Noun(NounVariant::Common).is_ancillary());
        assert!(!Speech::Verb(VerbForm::Godan("カ".to_string())).is_ancillary());
        assert!(!Speech::Adjective.is_ancillary());
        assert!(!Speech::Adverb.is_ancillary());
        assert!(!Speech::AdjectivalVerb.is_ancillary());
        assert!(!Speech::Verbatim.is_ancillary());
        assert!(!Speech::Conjunction.is_ancillary());
        assert!(Speech::Particle(ParticleType::Other).is_ancillary());
        assert!(Speech::AuxiliaryVerb.is_ancillary());
        assert!(!Speech::PreNounAdjectival.is_ancillary());
        assert!(!Speech::Counter.is_ancillary());
        assert!(Speech::Affix(AffixVariant::Prefix).is_ancillary());
    }

    #[test]
    fn test_affix_check() {
        // arrange

        // act

        // assert
        // 一通りの品詞をcheckする
        assert!(!Speech::Noun(NounVariant::Common).is_affix());
        assert!(!Speech::Verb(VerbForm::Godan("カ".to_string())).is_affix());
        assert!(!Speech::Adjective.is_affix());
        assert!(!Speech::Adverb.is_affix());
        assert!(!Speech::AdjectivalVerb.is_affix());
        assert!(!Speech::Verbatim.is_affix());
        assert!(!Speech::Conjunction.is_affix());
        assert!(!Speech::Particle(ParticleType::Other).is_affix());
        assert!(!Speech::AuxiliaryVerb.is_affix());
        assert!(!Speech::PreNounAdjectival.is_affix());
        assert!(!Speech::Counter.is_affix());
        assert!(Speech::Affix(AffixVariant::Prefix).is_affix());
    }

    #[test]
    fn test_noun_proper_check() {
        // arrange

        // act

        // assert
        // 一通りの品詞をcheckする
        assert!(!Speech::Noun(NounVariant::Common).is_noun_proper());
        assert!(Speech::Noun(NounVariant::Proper).is_noun_proper());
        assert!(!Speech::Verb(VerbForm::Godan("カ".to_string())).is_noun_proper());
        assert!(!Speech::Adjective.is_noun_proper());
        assert!(!Speech::Adverb.is_noun_proper());
        assert!(!Speech::AdjectivalVerb.is_noun_proper());
        assert!(!Speech::Verbatim.is_noun_proper());
        assert!(!Speech::Conjunction.is_noun_proper());
        assert!(!Speech::Particle(ParticleType::Other).is_noun_proper());
        assert!(!Speech::AuxiliaryVerb.is_noun_proper());
        assert!(!Speech::PreNounAdjectival.is_noun_proper());
        assert!(!Speech::Counter.is_noun_proper());
        assert!(!Speech::Affix(AffixVariant::Prefix).is_noun_proper());
    }

    #[test]
    fn guess_speech() {
        // arrange

        // act

        // assert
        assert_eq!(
            Speech::guess("食べない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("バ".to_string())),
                "食".to_string()
            )
        );
        assert_eq!(
            Speech::guess("受けない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("カ".to_string())),
                "受".to_string()
            )
        );
        assert_eq!(
            Speech::guess("告げない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("ガ".to_string())),
                "告".to_string()
            )
        );
        assert_eq!(
            Speech::guess("見せない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("サ".to_string())),
                "見".to_string()
            )
        );
        assert_eq!(
            Speech::guess("混ぜない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("ザ".to_string())),
                "混".to_string()
            )
        );
        assert_eq!(
            Speech::guess("捨てない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("タ".to_string())),
                "捨".to_string()
            )
        );
        assert_eq!(
            Speech::guess("茹でない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("ダ".to_string())),
                "茹".to_string()
            )
        );
        assert_eq!(
            Speech::guess("尋ねない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("ナ".to_string())),
                "尋".to_string()
            )
        );
        assert_eq!(
            Speech::guess("食べない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("バ".to_string())),
                "食".to_string()
            )
        );
        assert_eq!(
            Speech::guess("求めない"),
            (
                Speech::Verb(VerbForm::SimoIchidan("マ".to_string())),
                "求".to_string()
            )
        );
        assert_eq!(
            Speech::guess("書かない"),
            (
                Speech::Verb(VerbForm::Godan("カ".to_string())),
                "書".to_string()
            )
        );
        assert_eq!(
            Speech::guess("探さない"),
            (
                Speech::Verb(VerbForm::Godan("サ".to_string())),
                "探".to_string()
            )
        );
        assert_eq!(
            Speech::guess("勝たない"),
            (
                Speech::Verb(VerbForm::Godan("タ".to_string())),
                "勝".to_string()
            )
        );
        assert_eq!(
            Speech::guess("死なない"),
            (
                Speech::Verb(VerbForm::Godan("ナ".to_string())),
                "死".to_string()
            )
        );
        assert_eq!(
            Speech::guess("遊ばない"),
            (
                Speech::Verb(VerbForm::Godan("バ".to_string())),
                "遊".to_string()
            )
        );
        assert_eq!(
            Speech::guess("読まない"),
            (
                Speech::Verb(VerbForm::Godan("マ".to_string())),
                "読".to_string()
            )
        );
        assert_eq!(
            Speech::guess("切らない"),
            (
                Speech::Verb(VerbForm::Godan("ラ".to_string())),
                "切".to_string()
            )
        );
        assert_eq!(
            Speech::guess("問わない"),
            (
                Speech::Verb(VerbForm::Godan("ワ".to_string())),
                "問".to_string()
            )
        );

        assert_eq!(
            Speech::guess("大きい"),
            (Speech::Adjective, "大き".to_string())
        );
        assert_eq!(
            Speech::guess("綺麗だ"),
            (Speech::AdjectivalVerb, "綺麗".to_string())
        );
        assert_eq!(
            Speech::guess("一般"),
            (Speech::Noun(NounVariant::Common), "一般".to_string())
        );
    }
}
