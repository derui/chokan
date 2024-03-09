/// 辞書から各Entryを構築するための文法を定義する
///
/// 辞書の各エントリーは以下の形式で登録されている
///
/// 語幹かな\t語幹\t/品詞/\n
///
use peg::{error::ParseError, str::LineCol};

use crate::base::{
    entry::Entry,
    speech::{AffixVariant, NounVariant, Speech, VerbForm},
};

peg::parser! {

  grammar entry_parser() for str {
      rule eof() = ![_]
      rule any() = [_]
      rule space() = [' ' | '\t' ]
      rule no_space() = [^ ' ' | '\t' ]
      rule kana() -> String = n:$(['あ'..='ん' | 'ゐ' | 'ゃ' | 'ゅ' | 'ょ' | 'ぁ' | 'ぃ' | 'ぅ' | 'ぇ' | 'ぉ' | 'っ']) { n.to_string() }
      rule katakana() -> String = n:$(['ア' |'カ'| 'サ' | 'タ' |'ナ' | 'ハ'|'マ'|'ヤ'|'ワ' | 'ラ' | 'ダ' | 'バ' | 'ガ' | 'ザ']) { n.to_string() }

      rule noun() -> Speech = t:$("一般名詞" / "サ変名詞" / "固有名詞" ) {?
            match t {
                "一般名詞" => Ok(Speech::Noun(NounVariant::Common)),
                "サ変名詞" => Ok(Speech::Noun(NounVariant::Sahen)),
                "固有名詞" => Ok(Speech::Noun(NounVariant::Proper)),
                _ => Err("Invalid noun")
            }
      }
      rule verb() -> Speech = k:katakana() n:$("行五段" / "行四段" / "行上一" / "行下一" /"行上二" / "行下二" / "行変" ) {?
          let form = match n {
              "行五段" => Ok(VerbForm::Godan(k)),
              "行四段" => Ok(VerbForm::Yodan(k)),
              "行上一" => Ok(VerbForm::KamiIchidan(k)),
              "行下一" => Ok(VerbForm::SimoIchidan(k)),
              "行上二" => Ok(VerbForm::KamiNidan(k)),
              "行下二" => Ok(VerbForm::SimoNidan(k)),
              "行変" => Ok(VerbForm::Hen(k)),
              _ => Err("Invalid verb form")
          };

          form.map(|form|{Speech::Verb(form)})
      }

      rule adjective() -> Speech = "形容詞" { Speech::Adjective }
      rule adverb() -> Speech =  "副詞"  { Speech::Adverb }
      rule adjectival_verb() -> Speech = "形容動詞" { Speech::AdjectivalVerb }
      rule counter() -> Speech = "助数詞" { Speech::Counter }
      rule verbatim() -> Speech = "感動詞" { Speech::Verbatim }
      rule pre_noun_adjectival() -> Speech = "連体詞" { Speech::PreNounAdjectival }
      rule affix() -> Speech = s:$("接頭辞" / "接尾辞") {? match s {
            "接頭辞" => Ok(Speech::Affix(AffixVariant::Prefix)),
            "接尾辞" => Ok(Speech::Affix(AffixVariant::Suffix)),
            _ => Err("Invalid affix")
      } }
      rule speech() -> Speech = "/" n:(
          noun() / verb() / adjective() / adjectival_verb() / counter() / verbatim() / pre_noun_adjectival() / adverb() / affix()
      ) "/" {
          n
      }

      rule entry() -> Entry = k:$(kana()+) "\t" stem:$(no_space()+) "\t" ss:speech() {
          Entry::from_jisyo(k, stem, ss)
      }
      rule comment() = ";" any()*
      pub rule root() -> Option<Entry> = comment() {None} / n:entry() { Some(n) }

  }
}

/// 対象の一行を解析して、Entryを返す
pub fn parse_entry(s: &str) -> Result<Option<Entry>, ParseError<LineCol>> {
    entry_parser::root(s)
}

#[cfg(test)]
mod tests {
    use crate::base::speech::NounVariant;

    use super::*;

    #[test]
    fn test_parse_comment() {
        assert_eq!(parse_entry(";; abc"), Ok(None));
        assert_ne!(parse_entry("abc ; comments in back"), Ok(None));
    }

    #[test]
    fn test_parse_entry() {
        assert_eq!(
            parse_entry("を\t惜\t/形容詞/"),
            Ok(Some(Entry::from_jisyo("を", "惜", Speech::Adjective)))
        );

        assert_eq!(
            parse_entry("ゐ\t居\t/ラ行変/"),
            Ok(Some(Entry::from_jisyo(
                "ゐ",
                "居",
                Speech::Verb(VerbForm::Hen("ラ".to_string()))
            )))
        );

        assert_eq!(
            parse_entry("わせだどおり\t早稲田通り\t/一般名詞/"),
            Ok(Some(Entry::from_jisyo(
                "わせだどおり",
                "早稲田通り",
                Speech::Noun(NounVariant::Common)
            )))
        );

        assert_eq!(
            parse_entry("えんきょう\t円強\t/助数詞/"),
            Ok(Some(Entry::from_jisyo(
                "えんきょう",
                "円強",
                Speech::Counter
            )))
        );

        assert_eq!(
            parse_entry("おんみつ\t隠密\t/形容動詞/"),
            Ok(Some(Entry::from_jisyo(
                "おんみつ",
                "隠密",
                Speech::AdjectivalVerb
            )))
        );

        assert_eq!(
            parse_entry("ありがとう\t有り難う\t/感動詞/"),
            Ok(Some(Entry::from_jisyo(
                "ありがとう",
                "有り難う",
                Speech::Verbatim
            )))
        );

        assert_eq!(
            parse_entry("ふ\t不\t/接頭辞/"),
            Ok(Some(Entry::from_jisyo(
                "ふ",
                "不",
                Speech::Affix(AffixVariant::Prefix)
            )))
        );

        assert_eq!(
            parse_entry("じょう\t状\t/接尾辞/"),
            Ok(Some(Entry::from_jisyo(
                "じょう",
                "状",
                Speech::Affix(AffixVariant::Suffix)
            )))
        );
    }
}