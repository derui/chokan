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
    nfc_normalize(str)
        .chars()
        .map(|v| to_roma_sequence(v))
        .collect::<String>()
}

// NFC正規化した文字列を返す
fn nfc_normalize(str: &str) -> String {
    str.chars().nfc().collect::<String>()
}

// 1文字をローマ字に変換する
fn to_roma_sequence(c: char) -> String {
    let conversions = get_conversions();

    if let Some(v) = conversions.into_iter().find_map(|conv| conv.expand_roma(c)) {
        v
    } else {
        c.to_string()
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
    fn return_alphabet_converted_from_katakana() {
        assert_eq!("aiueo", convert("アイウエオ"));
    }

    #[test]
    fn return_alphabet_converted_from_katakana_and_hiragana() {
        assert_eq!("aiukaki", convert("アイうかき"));
    }
}
