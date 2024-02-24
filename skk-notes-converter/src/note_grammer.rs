// SKKの辞書におけるnotesの一行分の文法を定義する
//
// notesの構成は以下のようになっている。
//
// [見出しかな]+[送りアルファベット]space*/語幹;∥品詞[[送り仮名情報][活用]](/...)/\n
//

#[derive(Debug, PartialEq, Eq)]
struct Note {
    pub headword: String,
    pub okuri: String,
    pub entries: Vec<NoteEntry>,
}

#[derive(Debug, PartialEq, Eq)]
struct NoteEntry {
    stem: String,
    speech: NoteSpeech,
}

#[derive(Debug, PartialEq, Eq)]
enum NoteSpeech {
    Verb(VerbForm, Option<String>, Option<String>), // 活用形と、キャラクタクラス,を含めた動詞
    Adjective(Option<String>, Option<String>),      // 活用形を含む形容詞
    AdjectivalVerb(String),                         // 形容動詞
    Adverb(String),                                 // 副詞
    Noun(String, String),                           // 名詞の種類と、語幹の後に続く文字を含めた名詞
    Counter(String),                                // 助数詞
}

// 動詞の活用形
#[derive(Debug, PartialEq, Eq)]
enum VerbForm {
    Godan(String),       // 五段活用
    Yodan(String),       // 四段活用
    SimoIchidan(String), // 下一段活用
    KamiIchidan(String), // 上一段活用
    SimoNidan(String),   // 下二段活用
    KamiNidan(String),   // 上二段活用
    Hen(String),         // 変格活用
}

peg::parser! {

  grammar note_parser() for str {
      rule eof() = ![_]
      rule any() = [_]
      rule space() = [' ' | '\t' ]
      rule kana() -> String = n:$(['あ'..='ん' | 'ゐ' | 'ゃ' | 'ゅ' | 'ょ' | 'ぁ' | 'ぃ' | 'ぅ' | 'ぇ' | 'ぉ' | 'っ']) { n.to_string() }
      rule katakana() -> String = n:$(['ア'..='ン']) { n.to_string() }

    pub rule headword() -> String
      = n:kana()+ { n.concat() }

      pub rule okuri() -> String = n:$(['a'..='z']) { n.to_string() }
      // 語幹の漢字部分
      pub rule stem() -> String = n:$([^ ';']+) { n.to_string() }
      // 品詞部分
      rule fixed_okuri() -> String = "(-" n:$(kana()+) ")" { n.to_string() }
      rule char_class() -> String = "[" n:$(['a'..='z' | '>' | '<' | '#' | '*' | '-' | '(' | ')' | 'φ']+) "]" { n.to_string() }
      rule noun() -> NoteSpeech = t:$("サ変名詞" / "名詞" / "人称代名詞" / "疑問代名詞" / "連語" / "複合語" / "成句")  n:fixed_okuri() { NoteSpeech::Noun(t.to_string(), n.to_string()) }
      rule verb_form() -> VerbForm = k:katakana() n:$("行五段" / "行四段" / "行上一" / "行下一" /"行上二" / "行下二" / "変" ) {?
          match n {
              "行五段" => Ok(VerbForm::Godan(k)),
              "行四段" => Ok(VerbForm::Yodan(k)),
              "行上一" => Ok(VerbForm::KamiIchidan(k)),
              "行下一" => Ok(VerbForm::SimoIchidan(k)),
              "行上二" => Ok(VerbForm::KamiNidan(k)),
              "行下二" => Ok(VerbForm::SimoNidan(k)),
              "変" => Ok(VerbForm::Hen(k)),
              _ => Err("Invalid verb form")
          }
      }
      rule verb() -> NoteSpeech = t:verb_form() fix:fixed_okuri()? cl:char_class()? { NoteSpeech::Verb(t, cl, fix) }
      rule adjective() -> NoteSpeech = "形容詞" fix:fixed_okuri()? cl:char_class()? { NoteSpeech::Adjective(cl, fix) }
      rule adverb() -> NoteSpeech =  "副詞"  n:fixed_okuri() { NoteSpeech::Adverb(n.to_string()) }
      rule adjectival_verb() -> NoteSpeech = "形容動詞" n:fixed_okuri() { NoteSpeech::AdjectivalVerb(n.to_string()) }
      rule counter() -> NoteSpeech = "助数詞" cl:char_class() { NoteSpeech::Counter(cl.to_string()) }
      rule speech() -> NoteSpeech = "∥" "<base>"? n:(noun() / verb() / adjective() / counter()) { n }
      rule annotation() = ";" [^ '∥']*
      rule entry() -> NoteEntry = "/" s:stem() annotation() ss:speech() { NoteEntry { stem: s, speech: ss } }
      pub rule note() -> Note = h:headword() o:okuri()? space()+ entries:entry()+ "/" eof() { Note { headword: h, okuri: o.map(|v|v.to_string()).unwrap_or("".to_string()), entries: entries } }

      pub rule comment() = ";" any()*
  }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_headword() {
        assert_eq!(note_parser::headword("あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん"),
            Ok("あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをん".to_string())
        );
        assert_eq!(
            note_parser::headword("がぎぐげござじずぜぞだぢづでどばびぶべぼ"),
            Ok("がぎぐげござじずぜぞだぢづでどばびぶべぼ".to_string())
        );
        assert_eq!(
            note_parser::headword("しゃしゅしょぁぃぅぇぉっ"),
            Ok("しゃしゅしょぁぃぅぇぉっ".to_string())
        );
    }

    #[test]
    fn parse_okuri() {
        for (_, c) in "abcdefghijklmnopqrstuvwxyz".chars().enumerate() {
            assert_eq!(note_parser::okuri(&c.to_string()), Ok(c.to_string()));
        }

        // 送りのアルファベットは一つしかつかない
        assert_ne!(note_parser::okuri("abc"), Ok("a".to_string()));
    }

    #[test]
    fn parse_comment() {
        assert_eq!(note_parser::comment(";; abc"), Ok(()));
        assert_ne!(note_parser::comment("abc ; comments in back"), Ok(()));
    }

    #[test]
    fn parse_note() {
        assert_eq!(
            note_parser::note("をs /惜;∥形容詞(-しい)/"),
            Ok(Note {
                headword: "を".to_string(),
                okuri: "s".to_string(),
                entries: vec![NoteEntry {
                    stem: "惜".to_string(),
                    speech: NoteSpeech::Adjective(None, Some("しい".to_string()))
                }]
            })
        );

        assert_eq!(
            note_parser::note("ゐr /居;∥ラ変/"),
            Ok(Note {
                headword: "ゐ".to_string(),
                okuri: "r".to_string(),
                entries: vec![NoteEntry {
                    stem: "居".to_string(),
                    speech: NoteSpeech::Verb(VerbForm::Hen("ラ".to_string()), None, None)
                }]
            })
        );

        assert_eq!(note_parser::note("わらu /笑;∥<base>ワ行五段[wiueot(c)]/嗤;(jeer) 人の失敗を嗤う∥<base>ワ行五段[wiueot(c)]/"), Ok(Note {
            headword: "わら".to_string(),
            okuri: "u".to_string(),
            entries: vec![
                NoteEntry {
                    stem: "笑".to_string(),
                    speech: NoteSpeech::Verb(VerbForm::Godan("ワ".to_string()), Some("wiueot(c)".to_string()), None)
                },
                NoteEntry {
                    stem: "嗤".to_string(),
                    speech: NoteSpeech::Verb(VerbForm::Godan("ワ".to_string()), Some("wiueot(c)".to_string()), None)
                },
            ]
        }));

        assert_eq!(
            note_parser::note("わせだどおr /早稲田通;∥名詞(-り)/"),
            Ok(Note {
                headword: "わせだどお".to_string(),
                okuri: "r".to_string(),
                entries: vec![NoteEntry {
                    stem: "早稲田通".to_string(),
                    speech: NoteSpeech::Noun("名詞".to_string(), "り".to_string())
                }]
            })
        );

        assert_eq!(
            note_parser::note("わけあたe /分け与;∥ア行下一(-える)/"),
            Ok(Note {
                headword: "わけあた".to_string(),
                okuri: "e".to_string(),
                entries: vec![NoteEntry {
                    stem: "分け与".to_string(),
                    speech: NoteSpeech::Verb(
                        VerbForm::SimoIchidan("ア".to_string()),
                        None,
                        Some("える".to_string())
                    )
                }]
            })
        );

        assert_eq!(
            note_parser::note("えんきょう /円強;∥助数詞[#]/"),
            Ok(Note {
                headword: "えんきょう".to_string(),
                okuri: "".to_string(),
                entries: vec![NoteEntry {
                    stem: "円強".to_string(),
                    speech: NoteSpeech::Counter("#".to_string())
                }]
            })
        )
    }

    #[test]
    fn parse_error_derived() {
        assert!(
            note_parser::note("わらe /笑;∥<derived>/").is_err(),
            "should be error if derived"
        )
    }
}
