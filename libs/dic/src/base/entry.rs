use std::fmt::{self, Display};

use super::{speech::Speech, word::Word};

/// 辞書における一単語を表現する型
/// 語幹と活用形は辞書でのみ必要になる属性であるため、基本的にはWordを利用する必要がある
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Entry {
    /// 語幹
    stem: String,
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
            stem_reading: reading.to_string(),
            speech,
        }
    }

    /// 品詞を推測してEntryを生成する
    ///
    /// 品詞の推定のため、reading/kanjiはいずれも否定形または形容詞・形容動詞の基本形である必要がある。
    ///
    /// # Arguments
    /// * `reading` - 読み
    /// * `kanji` - 漢字
    ///
    /// # Returns
    /// 推測された品詞を持つEntry
    pub fn new_guessed(reading: &str, kanji: &str) -> Entry {
        let (speech, stem) = Speech::guess(kanji);

        let len_diff = kanji.len() - stem.len();
        if len_diff > 0 {
            Entry {
                stem,
                stem_reading: reading[0..(reading.len() - len_diff)].to_string(),
                speech,
            }
        } else {
            Entry {
                stem: stem.to_string(),
                stem_reading: reading.to_string(),
                speech,
            }
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
        let forms = &val.speech.to_forms(&val.stem, &val.stem_reading);

        for (formed, form_reading) in forms {
            words.push(Word::new(formed, form_reading, val.speech.clone()));
        }

        words
    }
}

impl From<&Entry> for Vec<Word> {
    /// Entryから活用した形式一覧をWordのリストに変換する
    fn from(val: &Entry) -> Self {
        let mut words = Vec::new();
        let forms = &val.speech.to_forms(&val.stem, &val.stem_reading);

        for (formed, form_reading) in forms {
            words.push(Word::new(formed, form_reading, val.speech.clone()));
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

    #[test]
    fn display_kahen_entry() {
        // Arrange
        let entry = Entry::from_jisyo("く", "来", Speech::Verb(VerbForm::Hen("カ".to_string())));

        // Act
        let words: Vec<Word> = entry.into();

        // Assert
        assert!(
            words.contains(&Word::new(
                "来",
                "こ",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "こ does not contains"
        );
        assert!(
            words.contains(&Word::new(
                "来",
                "き",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "き does not contains"
        );
        assert!(
            words.contains(&Word::new(
                "来る",
                "くる",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "くる does not contains"
        );
        assert!(
            words.contains(&Word::new(
                "来れ",
                "くれ",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "くれ does not contains"
        );
        assert!(
            words.contains(&Word::new(
                "来い",
                "こい",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "こい does not contains"
        );
    }

    #[test]
    fn display_kahen_entry2() {
        // Arrange
        let entry = Entry::from_jisyo(
            "かえってく",
            "帰って来",
            Speech::Verb(VerbForm::Hen("カ".to_string())),
        );

        // Act
        let words: Vec<Word> = entry.into();

        // Assert
        assert!(
            words.contains(&Word::new(
                "帰って来",
                "かえってこ",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "こ does not contains"
        );
        assert!(
            words.contains(&Word::new(
                "帰って来",
                "かえってき",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "き does not contains"
        );
        assert!(
            words.contains(&Word::new(
                "帰って来る",
                "かえってくる",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "くる does not contains"
        );
        assert!(
            words.contains(&Word::new(
                "帰って来れ",
                "かえってくれ",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "くれ does not contains"
        );
        assert!(
            words.contains(&Word::new(
                "帰って来い",
                "かえってこい",
                Speech::Verb(VerbForm::Hen("カ".to_string()))
            )),
            "こい does not contains"
        );
    }

    #[test]
    fn guess_entry() {
        // Arrange

        // Act
        let entry = Entry::new_guessed("たべない", "食べない");

        // Assert
        assert_eq!(
            entry,
            Entry::from_jisyo(
                "た",
                "食",
                Speech::Verb(VerbForm::SimoIchidan("バ".to_string()))
            )
        )
    }
}
