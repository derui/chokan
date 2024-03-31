use dic::base::{
    entry::Entry,
    speech::{NounVariant, Speech},
};
use peg::{error::ParseError, str::LineCol};

/*
名詞相当のエントリ。送り有りはすべて無視する
*/
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Noun {
    reading: String,
    entries: Vec<String>,
}

impl Noun {
    /*
    単漢字を、全体の辞書で利用するentry形式に変換する
     */
    pub(crate) fn to_entries(&self) -> Vec<Entry> {
        self.entries
            .iter()
            .map(|e| Entry::from_jisyo(&self.reading, e, Speech::Noun(NounVariant::Common)))
            .collect()
    }
}

/// 対象の一行を解析して、Noteを返す
pub fn parse_nouns(s: &str) -> Result<Option<Noun>, ParseError<LineCol>> {
    let entry = skk_dic_parser::parse_skk_entry(s)?;
    if let Some(entry) = entry {
        // 一文字ではない場合は単漢字とみなさない
        if entry.okuri().is_some() {
            Ok(None)
        } else {
            Ok(Some(Noun {
                reading: entry.reading(),
                entries: entry.words(),
            }))
        }
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn ignore_okuri_ari_entry() {
        assert_eq!(
            parse_nouns("むち /鞭/無知/"),
            Ok(Some(Noun {
                reading: "むち".to_string(),
                entries: vec!["鞭".to_string(), "無知".to_string()]
            }))
        );

        assert_eq!(parse_nouns("むちf /無知/"), Ok(None));
    }
}
