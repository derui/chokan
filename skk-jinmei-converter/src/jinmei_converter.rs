use dic::base::{
    entry::Entry,
    speech::{NounVariant, Speech},
};
use peg::{error::ParseError, str::LineCol};

/*
固有名詞相当のエントリ
*/
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Proper {
    reading: String,
    entries: Vec<String>,
}

impl Proper {
    /*
    人名を、全体の辞書で利用するentry形式に変換する
     */
    pub(crate) fn to_entries(&self) -> Vec<Entry> {
        self.entries
            .iter()
            .map(|e| Entry::from_jisyo(&self.reading, e, Speech::Noun(NounVariant::Proper)))
            .collect()
    }
}

/// 対象の一行を解析して、Noteを返す
pub fn parse_propers(s: &str) -> Result<Option<Proper>, ParseError<LineCol>> {
    let entry = skk_dic_parser::parse_skk_entry(s)?;
    if let Some(entry) = entry {
        // 人名の場合は、読みとwordsだけで作成できる
        Ok(Some(Proper {
            reading: entry.reading(),
            entries: entry.words(),
        }))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn parser_jinmei_entry() {
        assert_eq!(
            parse_propers("あいざわ /相澤; test/相沢/"),
            Ok(Some(Proper {
                reading: "あいざわ".to_string(),
                entries: vec!["相澤".to_string(), "相沢".to_string()]
            }))
        );
    }
}
