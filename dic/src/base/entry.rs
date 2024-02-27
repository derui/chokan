use std::{
    collections::HashSet,
    fmt::{self, Display},
};

use super::{speech::Speech, word::Word};

/// 辞書における一単語を表現する型
/// 語幹と活用形は辞書でのみ必要になる属性であるため、基本的にはWordを利用する必要がある
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Entry {
    /// 語幹
    stem: String,
    /// 活用形
    forms: HashSet<String>,
    /// 語幹の読み。活用形はすべてひらがななので、語幹の読みだけを持つ
    pub stem_reading: String,
    /// 品詞。動詞の活用形はformsで表現されているため、ここでは不要としている
    pub speech: Speech,
}

impl Entry {
    /// 読みと漢字、品詞からEntryを生成する
    pub fn from_jisyo(reading: &str, kanji: &str, speech: Speech) -> Entry {
        Entry {
            stem: kanji.to_string(),
            forms: speech.to_forms(reading),
            stem_reading: reading.to_string(),
            speech,
        }
    }
}

impl Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kana = &self.stem_reading;
        let kanji = &self.stem;
        write!(f, "{}\t{}\t/{}/", kana, kanji, self.speech)
    }
}

impl From<Entry> for Vec<Word> {
    /// Entryから活用した形式一覧をWordのリストに変換する
    fn from(val: Entry) -> Self {
        let mut words = Vec::new();
        let forms = &val.forms;

        for form in forms {
            let reading = format!("{}{}", val.stem_reading, form);
            words.push(Word::new(
                &format!("{}{}", val.stem, form),
                &reading,
                val.speech.clone(),
            ));
        }

        words
    }
}

#[cfg(test)]
mod tests {
    use crate::base::speech::VerbForm;

    use super::*;

    #[test]
    fn test_entry_to_word() {
        // Arrange
        let verb = Speech::Verb(VerbForm::SimoIchidan("バ".to_string()));
        let entry = Entry::from_jisyo("た", "食", verb.clone());

        // Act
        let words: Vec<Word> = entry.into();

        // Assert
        assert_eq!(words.len(), 1);
        assert!(
            words.contains(&Word::new("食べ", "たべ", verb.clone())),
            "word doesnt contains"
        );
    }

    #[test]
    fn contains_only_stem() {
        // Arrange
        let entry = Entry::from_jisyo("きれい", "綺麗", Speech::AdjectivalVerb);

        // Act
        let words: Vec<Word> = entry.into();

        // Assert
        assert_eq!(words.len(), 6);
        assert!(
            words.contains(&Word::new("綺麗な", "きれいな", Speech::AdjectivalVerb)),
            "きれいな does not contains"
        );
        assert!(
            words.contains(&Word::new("綺麗だ", "きれいだ", Speech::AdjectivalVerb)),
            "きれいな does not contains"
        );
        assert!(
            words.contains(&Word::new("綺麗だっ", "きれいだっ", Speech::AdjectivalVerb)),
            "きれいな does not contains"
        );
        assert!(
            words.contains(&Word::new("綺麗なら", "きれいなら", Speech::AdjectivalVerb)),
            "きれいな does not contains"
        );
        assert!(
            words.contains(&Word::new("綺麗で", "きれいで", Speech::AdjectivalVerb)),
            "きれいで does not contains"
        );
        assert!(
            words.contains(&Word::new("綺麗に", "きれいに", Speech::AdjectivalVerb)),
            "きれいに does not contains"
        );
    }

    #[test]
    fn display_adjective_entry() {
        // Arrange
        let entry = Entry::from_jisyo("おおき", "大き", Speech::Adjective);

        // Act
        let actual = format!("{}", entry);

        // Assert
        assert_eq!(actual, "おおき\t大き\t/形容詞/");
    }

    #[test]
    fn display_verb_entry() {
        // Arrange
        let entry = Entry::from_jisyo("ひ", "引", Speech::Verb(VerbForm::Godan("カ".to_string())));

        // Act
        let actual = format!("{}", entry);

        // Assert
        assert_eq!(actual, "ひ\t引\t/カ行五段/");
    }
}
