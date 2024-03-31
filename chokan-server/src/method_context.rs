use std::sync::Mutex;

use chokan_dic::ChokanDictionary;
use kkc::frequency::ConversionFrequency;

pub struct MethodContext {
    /// 全体で利用する辞書
    pub dictionary: ChokanDictionary,
    /// 変換頻度
    pub frequency: Mutex<ConversionFrequency>,
}
