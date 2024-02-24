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

#[derive(Debug, PartialEq, Eq, Clone)]
struct NoteEntry {
    stem: String,
    speech: NoteSpeech,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum NoteSpeech {
    Verb(VerbForm, Option<Okuri>), // 活用形と、キャラクタクラス,を含めた動詞
    Adjective(Option<Okuri>),      // 活用形を含む形容詞
    AdjectivalVerb(Okuri),         // 形容動詞
    Adverb(Okuri),                 // 副詞
    Noun(String, Option<Okuri>),   // 名詞の種類と、語幹の後に続く文字を含めた名詞
    Counter(Okuri),                // 助数詞
}

// 動詞の活用形
#[derive(Debug, PartialEq, Eq, Clone)]
enum VerbForm {
    Godan(String),       // 五段活用
    Yodan(String),       // 四段活用
    SimoIchidan(String), // 下一段活用
    KamiIchidan(String), // 上一段活用
    SimoNidan(String),   // 下二段活用
    KamiNidan(String),   // 上二段活用
    Hen(String),         // 変格活用
}

// 送り仮名の情報
#[derive(Debug, PartialEq, Eq, Clone)]
enum Okuri {
    Fixed(String),     // -いる、-えるなどの固定の送り仮名
    CharClass(String), // []で囲まれたキャラクタクラス
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

      pub rule okuri_alpha() -> String = n:$(['a'..='z']) { n.to_string() }
      // 語幹の漢字部分
      pub rule stem() -> String = n:$([^ ';']+) { n.to_string() }
      // 品詞部分
      rule fixed_okuri() -> Okuri = "(-" n:$(kana()+) ")" { Okuri::Fixed(n.to_string()) }
      rule char_class() -> Okuri = "[" n:$(['a'..='z' | '>' | '<' | '#' | '*' | '-' | '(' | ')' | 'φ']+) "]" { Okuri::CharClass(n.to_string()) }
      rule okuri() -> Okuri = n:(fixed_okuri() / char_class()) { n }
            rule noun() -> NoteSpeech = t:$("サ変名詞" / "名詞" / "人称代名詞" / "疑問代名詞" / "連語" / "複合語" / "成句")  o:okuri()? { NoteSpeech::Noun(t.to_string(), o) }
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
      rule verb() -> NoteSpeech = t:verb_form() o:okuri()?  { NoteSpeech::Verb(t, o) }
      rule adjective() -> NoteSpeech = "形容詞" o:okuri()? { NoteSpeech::Adjective(o) }
      rule adverb() -> NoteSpeech =  "副詞"  o:okuri() { NoteSpeech::Adverb(o) }
      rule adjectival_verb() -> NoteSpeech = "形容動詞" o:okuri() { NoteSpeech::AdjectivalVerb(o) }
      rule counter() -> NoteSpeech = "助数詞" o:okuri() { NoteSpeech::Counter(o) }
      rule speech() -> Vec<NoteSpeech> = "∥" "<base>"? n:(noun() / verb() / adjective() / adjectival_verb() / counter()) ** "," { n }
      rule annotation() = ";" [^ '∥']*
      rule entry() -> Vec<NoteEntry> = "/" s:stem() annotation() ss:speech() { ss.iter().map(|v| NoteEntry { stem: s.clone(), speech: v.clone() }).collect() }
      pub rule note() -> Note = h:headword() o:okuri_alpha()? space()+ entries:entry()+ "/" eof() {
          let entries = entries.iter().flatten().cloned().collect();
          let okuri = o.map(|v|v.to_string()).unwrap_or("".to_string());
          Note { headword: h, okuri , entries  } }

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
            assert_eq!(note_parser::okuri_alpha(&c.to_string()), Ok(c.to_string()));
        }

        // 送りのアルファベットは一つしかつかない
        assert_ne!(note_parser::okuri_alpha("abc"), Ok("a".to_string()));
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
                    speech: NoteSpeech::Adjective(Some(Okuri::Fixed("しい".to_string())))
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
                    speech: NoteSpeech::Verb(VerbForm::Hen("ラ".to_string()), None)
                }]
            })
        );

        assert_eq!(note_parser::note("わらu /笑;∥<base>ワ行五段[wiueot(c)]/嗤;(jeer) 人の失敗を嗤う∥<base>ワ行五段[wiueot(c)]/"), Ok(Note {
            headword: "わら".to_string(),
            okuri: "u".to_string(),
            entries: vec![
                NoteEntry {
                    stem: "笑".to_string(),
                    speech: NoteSpeech::Verb(VerbForm::Godan("ワ".to_string()), Some(Okuri::CharClass("wiueot(c)".to_string())))
                },
                NoteEntry {
                    stem: "嗤".to_string(),
                    speech: NoteSpeech::Verb(VerbForm::Godan("ワ".to_string()), Some(Okuri::CharClass("wiueot(c)".to_string())))
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
                    speech: NoteSpeech::Noun(
                        "名詞".to_string(),
                        Some(Okuri::Fixed("り".to_string()))
                    )
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
                        Some(Okuri::Fixed("える".to_string()))
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
                    speech: NoteSpeech::Counter(Okuri::CharClass("#".to_string()))
                }]
            })
        );

        assert_eq!(
            note_parser::note("おんみつ /隠密;∥形容動詞[φdn(s)],名詞/"),
            Ok(Note {
                headword: "おんみつ".to_string(),
                okuri: "".to_string(),
                entries: vec![
                    NoteEntry {
                        stem: "隠密".to_string(),
                        speech: NoteSpeech::AdjectivalVerb(Okuri::CharClass("φdn(s)".to_string()))
                    },
                    NoteEntry {
                        stem: "隠密".to_string(),
                        speech: NoteSpeech::Noun("名詞".to_string(), None)
                    }
                ]
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
