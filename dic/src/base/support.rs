use std::backtrace;

/// internal support
use crate::base::speech::Speech;

/// 辞書にある読みから、読み自体の語幹を返す
/// 渡される読みは、辞書系と一致する必要がある
pub fn reading_to_stem_reading(reading: &str, speech: &Speech) -> String {
    match speech {
        Speech::Verb(form) => {
            let base_suffix = form.to_dictionary_okuri();

            if reading.ends_with(base_suffix) {
                reading[..(reading.len() - base_suffix.len())].to_string()
            } else {
                reading.to_string()
            }
        }
        Speech::Adjective => reading[..(reading.len() - "い".len())].to_string(),
        Speech::AdjectivalVerb => reading[..(reading.len() - "だ".len())].to_string(),
        Speech::Noun => reading.to_string(),
        Speech::Adverb => reading.to_string(),
        Speech::Verbatim => reading.to_string(),
        Speech::Conjunction => reading.to_string(),
        Speech::Particle(_) => reading.to_string(),
        Speech::AuxiliaryVerb => reading.to_string(),
        Speech::PreNounAdjectival => reading.to_string(),
        Speech::Counter => reading.to_string(),
    }
}

/// 辞書形の変換系から、語幹を返す
/// ここでは、特に動詞の活用形にたいする処理が多い
pub fn kanji_to_stem(kanji: &str, speech: &Speech) -> String {
    match speech {
        Speech::Verb(form) => {
            let base_suffix = form.to_dictionary_okuri();

            // 語幹が送り仮名を含む場合、一文字だけになってしまうため、その場合は先頭の一文字だけ返す。
            // このケースは、ほぼ二文字の単語で構成されている。
            if kanji.len() == base_suffix.len() {
                kanji[..1].to_string()
            } else if kanji.ends_with(base_suffix) {
                kanji[..(kanji.len() - base_suffix.len())].to_string()
            } else {
                kanji.to_string()
            }
        }
        Speech::Adjective => kanji[..(kanji.len() - "い".len())].to_string(),
        Speech::AdjectivalVerb => kanji[..(kanji.len() - "だ".len())].to_string(),
        _ => kanji.to_string(),
    }
}
