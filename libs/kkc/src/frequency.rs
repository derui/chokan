use std::collections::HashMap;

use chrono::{Duration, TimeDelta};
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

/// 変換結果を保持するstruct
///
/// 頻度と最後に発生したepoch秒を保存している
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
struct Frequency {
    count: u64,
    last_occurrance: i64,
}

/// 変換結果の頻度を保存するための構造体
///
/// 頻度は、変換結果の文字列をキーとして、その変換結果を確定した回数を保存する。
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub struct ConversionFrequency {
    /// 変換結果と回数を保存する
    frequencies: HashMap<ConvertedResult, Frequency>,
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
    /// * `word` - 更新する変換結果
    /// * `context` - 対象のcontext
    /// * `now` - 変換が行われた時刻。msec単位
    ///
    pub fn update_word(&mut self, word: &str, context: &Context, now: i64) {
        let frequency = self
            .frequencies
            .entry(ConvertedResult::Word {
                context: context.clone(),
                word: word.to_string(),
            })
            .or_insert(Frequency {
                count: 0,
                last_occurrance: 0,
            });
        frequency.count += 1;
        frequency.last_occurrance = now
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

        self.frequencies.get(&result).map_or(0, |freq| freq.count)
    }

    /// 指定した時刻の時点より `expiration` 以上前に利用されたものを削除する
    ///
    /// # Arguments
    /// * `now` - 現在時点の時刻。msec単位であること
    /// * `expiration` - 有効期限を表わす。msec単位であること
    pub fn expire_frequencies(&mut self, now: i64, expiration: i64) {
        let mut cloned = self.frequencies.clone();

        let now = Duration::milliseconds(now);
        let expired_duration = Duration::milliseconds(expiration);

        for (key, value) in self.frequencies.iter() {
            let occurred = Duration::milliseconds(value.last_occurrance);

            let v = now - occurred;
            if v > expired_duration {
                cloned.remove(key);
            }
        }

        self.frequencies = cloned;
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
        obj.update_word("foo", &Context::normal(), 10);

        // act
        let ret = obj.get_frequency_of_word("foo", &Context::normal());
        let other_context = obj.get_frequency_of_word("foo", &Context::foreign_word());

        // assert
        assert_eq!(ret, 1);
        assert_eq!(other_context, 0);
    }

    #[test]
    fn delete_expired_entry() {
        // arrange
        let mut obj = ConversionFrequency::new();
        obj.update_word("foo", &Context::normal(), 10);
        obj.update_word("bar", &Context::normal(), 11);
        obj.update_word("baz", &Context::normal(), 12);

        // act
        obj.expire_frequencies(13, 2);

        // assert
        let ctx = Context::normal();
        assert_eq!(0, obj.get_frequency_of_word("foo", &ctx), "foo");
        assert_eq!(1, obj.get_frequency_of_word("bar", &ctx), "bar");
        assert_eq!(1, obj.get_frequency_of_word("baz", &ctx), "baz");
    }
}
