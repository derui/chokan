use std::{
    sync::{Arc, Mutex},
    time::SystemTime,
};

use chokan_dic::ChokanDictionary;
use chrono::Utc;

use crate::user_pref::UserPref;

pub struct MethodContext {
    /// 全体で利用する辞書
    pub dictionary: Arc<Mutex<ChokanDictionary>>,

    /// ユーザーの設定として管理するもの
    pub user_pref: Arc<Mutex<UserPref>>,

    /// 現在時刻を取得するための関数
    pub now: Arc<dyn Fn() -> i64 + Sync + Send>,
}

impl MethodContext {
    /// 新規に[MethodContext]を生成する
    pub fn new(dictionary: Arc<Mutex<ChokanDictionary>>, user_pref: Arc<Mutex<UserPref>>) -> Self {
        MethodContext {
            dictionary: dictionary.clone(),
            user_pref: user_pref.clone(),
            now: Arc::new(move || {
                let time = Utc::now();
                time.timestamp_millis()
            }),
        }
    }
}
