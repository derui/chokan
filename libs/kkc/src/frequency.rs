use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::context::Context;

/// 変換結果の頻度を保存するための構造体
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
enum ConvertedResult {
    Word {
        /// 確定したときのContext
        context: Context,
        /// 確定した変換後の単語
        word: String,
    },
}

/// 変換結果の頻度を保存するための構造体
///
/// 頻度は、変換結果の文字列をキーとして、その変換結果を確定した回数を保存する。
#[derive(Serialize, Deserialize, Debug)]
pub struct ConversionFrequency {
    /// 変換結果と回数を保存する
    frequencies: HashMap<ConvertedResult, u64>,
}

impl Default for ConversionFrequency {
    fn default() -> Self {
        Self::new()
    }
}

impl ConversionFrequency {
    /// 新しい[ConversionFrequency]を生成する
    pub fn new() -> ConversionFrequency {
        ConversionFrequency {
            frequencies: HashMap::new(),
        }
    }

    /// 頻度を更新する
    ///
    /// # Arguments
    /// * `result` - 更新する変換結果
    ///
    pub fn update_word(&mut self, word: &str, context: &Context) {
        let count = self
            .frequencies
            .entry(ConvertedResult::Word {
                context: context.clone(),
                word: word.to_string(),
            })
            .or_insert(0);
        *count += 1;
    }

    /// 指定した変換結果の頻度を取得する
    ///
    /// # Arguments
    /// * `word` - 単語
    /// * `context` - コンテキスト
    ///
    /// # Returns
    /// 指定した変換結果の頻度。まだ変換されたことがない場合は0
    pub fn get_frequency_of_word(&self, word: &str, context: &Context) -> u64 {
        let result = ConvertedResult::Word {
            context: context.clone(),
            word: word.to_string(),
        };

        self.frequencies.get(&result).map_or(0, |count| *count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_frequency() {
        // arrange
        let obj = ConversionFrequency::new();
        // act
        let ret = obj.get_frequency_of_word("foo", &Context::normal());

        // assert
        assert_eq!(ret, 0);
    }

    #[test]
    fn update_frequency() {
        // arrange
        let mut obj = ConversionFrequency::new();
        obj.update_word("foo", &Context::normal());

        // act
        let ret = obj.get_frequency_of_word("foo", &Context::normal());
        let other_context = obj.get_frequency_of_word("foo", &Context::foreign_word());

        // assert
        assert_eq!(ret, 1);
        assert_eq!(other_context, 0);
    }
}
