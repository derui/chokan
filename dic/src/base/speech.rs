use std::fmt::Display;

// 単語における品詞
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Speech {
    Noun,              // 名詞
    Verb(VerbForm),    // 動詞。引数は辞書系で利用する活用
    Adjective,         // 形容詞
    Adverb,            // 副詞
    AdjectivalVerb,    // 形容動詞
    Verbatim,          // 感動詞
    Conjunction,       // 接続詞
    Particle,          // 助詞
    AuxiliaryVerb,     // 助動詞
    PreNounAdjectival, // 連体詞
    Counter,           // 助数詞
}

impl Display for Speech {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Speech::Noun => write!(f, "名詞"),
            Speech::Verb(form) => write!(f, "{}", form),
            Speech::Adjective => write!(f, "形容詞"),
            Speech::Adverb => write!(f, "副詞"),
            Speech::AdjectivalVerb => write!(f, "形容動詞"),
            Speech::Verbatim => write!(f, "感動詞"),
            Speech::Conjunction => write!(f, "接続詞"),
            Speech::Particle => write!(f, "助詞"),
            Speech::AuxiliaryVerb => write!(f, "助動詞"),
            Speech::PreNounAdjectival => write!(f, "連体詞"),
            Speech::Counter => write!(f, "助数詞"),
        }
    }
}

/// 動詞の活用形
/// 引数は、各活用で利用する段である
#[derive(Debug, PartialEq, Eq, Clone)]
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
    /// 辞書形に対応する活用形を返す
    pub fn to_dictionary_form(&self, stem: &str) -> String {
        let okuri = match self {
            VerbForm::Godan(row) => match row.as_str() {
                "ア" => "う",
                "カ" => "く",
                "サ" => "す",
                "タ" => "つ",
                "ナ" => "ぬ",
                "ハ" => "ふ",
                "マ" => "む",
                "ラ" => "る",
                "ワ" => "う",
                _ => panic!("Can not get okuri for godan verb with {}", row),
            },

            VerbForm::Yodan(row) => match row.as_str() {
                "ア" => "う",
                "カ" => "く",
                "サ" => "る",
                "タ" => "つ",
                "ナ" => "ぬ",
                "ハ" => "ふ",
                "マ" => "む",
                "ラ" => "る",
                "ワ" => "う",
                _ => panic!("Can not get okuri for godan verb with {}", row),
            },
            VerbForm::SimoIchidan(row) => match row.as_str() {
                "ア" => "える",
                "カ" => "ける",
                "サ" => "せる",
                "タ" => "てる",
                "ナ" => "ねる",
                "ハ" => "へる",
                "マ" => "める",
                "ラ" => "れる",
                _ => panic!("Can not get okuri for godan verb with {}", row),
            },
            VerbForm::KamiIchidan(row) => match row.as_str() {
                "ア" => "いる",
                "カ" => "きる",
                "タ" => "ちる",
                // ハ行上一は、「にる」のみ
                "ナ" => "る",
                // ハ行上一は、「ひる」のみ
                "ハ" => "る",
                "マ" => "みる",
                "ラ" => "りる",
                _ => panic!("Can not get okuri for godan verb with {}", row),
            },
            VerbForm::SimoNidan(row) => match row.as_str() {
                // ア行下二は、「得る」のみ
                "ア" => "う",
                "カ" => "く",
                "サ" => "す",
                "タ" => "つ",
                "ナ" => "ぬ",
                "ハ" => "ふ",
                "マ" => "む",
                "ラ" => "る",
                "ヤ" => "ゆ",
                "ワ" => "う",
                _ => panic!("Can not get okuri for godan verb with {}", row),
            },
            VerbForm::KamiNidan(row) => match row.as_str() {
                "カ" => "く",
                "タ" => "つ",
                "ハ" => "ふ",
                "マ" => "む",
                "ヤ" => "ゆ",
                "ラ" => "る",
                "ワ" => "る",
                _ => panic!("Can not get okuri for godan verb with {}", row),
            },
            VerbForm::Hen(row) => match row.as_str() {
                "カ" => "る",
                "サ" => "る",
                _ => panic!("Can not get okuri for henkaku verb with {}", row),
            },
        };

        format!("{}{}", stem, okuri)
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
        assert_eq!(form.to_dictionary_form("ひ"), "ひく");
    }
}
