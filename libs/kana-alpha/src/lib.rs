use conversion::get_conversions;
use unicode_normalization::UnicodeNormalization;

mod conversion;

/// かな混りの文字列をアルファベットに変換する
///
/// アルファベットがまざっていても変換が可能。
///
/// # Arguments
/// - `str` - かな混じりの文字列
///
/// # Returns
/// - アルファベットのみの文字列
pub fn convert(str: &str) -> String {
    let mut normalized = nfc_normalize(str);
    let mut ret = String::new();

    while !normalized.is_empty() {
        let (v, rest) = to_roma_sequence(&normalized);

        normalized = rest;
        ret.push_str(&v);
    }

    ret
}

// NFC正規化した文字列を返す
fn nfc_normalize(str: &str) -> String {
    str.chars().nfc().collect::<String>()
}

// 1文字をローマ字に変換する
fn to_roma_sequence(s: &str) -> (String, String) {
    let conversions = get_conversions();

    let mut conversions = conversions
        .into_iter()
        .filter_map(|conv| conv.expand_roma(s))
        .collect::<Vec<_>>();
    conversions.sort_by(|(_, s1), (_, s2)| s1.cmp(s2));
    conversions.reverse();

    if let Some((v, len)) = conversions.get(0) {
        let rest = s.chars().skip(*len).collect();
        (v.clone(), rest)
    } else {
        let v = s.to_string();
        let ret = v.chars().take(1).collect();
        let rest = v.chars().skip(1).collect();
        (ret, rest)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn return_same_string_if_ascii_only() {
        assert_eq!("ascii_only", convert("ascii_only"));
    }

    #[test]
    fn return_alphabet_converted_from_hiragana() {
        assert_eq!("aiueo", convert("あいうえお"));
    }

    #[test]
    fn should_be_nfc() {
        assert_eq!("papipupepo", convert("ぱぴぷぺぽ"));
    }

    #[test]
    fn return_alphabet_converted_from_katakana() {
        assert_eq!("aiueo", convert("アイウエオ"));
    }

    #[test]
    fn return_alphabet_converted_from_katakana_and_hiragana() {
        assert_eq!("aiukaki", convert("アイうかき"));
    }

    #[test]
    fn use_longest_word() {
        assert_eq!("syain", convert("しゃいん"));
        assert_eq!("program", convert("pろgらm"));
        assert_eq!("implementation", convert("いmpleめnたちおん"));
    }

    #[test]
    fn use_foreign_roman() {
        assert_eq!("buffer", convert("ぶっふぇr"));
    }
}
