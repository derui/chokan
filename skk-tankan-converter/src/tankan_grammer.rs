use dic::base::{
    entry::Entry,
    speech::{NounVariant, Speech},
};
// SKKの辞書におけるnotesの一行分の文法を定義する
//
// notesの構成は以下のようになっている。
//
// [見出しかな]+[送りアルファベット]space*/語幹;∥品詞[[送り仮名情報][活用]](/...)/\n
//
use peg::{error::ParseError, str::LineCol};

/*
単漢字のエントリ。送り有りはすべて無視する
*/
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tankan {
    reading: String,
    entries: Vec<String>,
}

impl Tankan {
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
pub fn parse_tankan(s: &str) -> Result<Option<Tankan>, ParseError<LineCol>> {
    let tankan = skk_dic_parser::parse_skk_entry(s)?;
    if let Some(entry) = tankan {
        // 一文字ではない場合は単漢字とみなさない
        let entries = entry
            .words()
            .iter()
            .filter(|&v| v.chars().collect::<Vec<char>>().len() == 1)
            .cloned()
            .collect::<Vec<_>>();

        if entries.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Tankan {
                reading: entry.reading(),
                entries,
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
    fn ignore_multi_character_word() {
        assert_eq!(
            parse_tankan("むち /鞭/無知/"),
            Ok(Some(Tankan {
                reading: "むち".to_string(),
                entries: vec!["鞭".to_string()]
            }))
        );

        assert_eq!(parse_tankan("むち /無知/"), Ok(None));
    }
}
