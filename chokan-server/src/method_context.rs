use std::sync::{Arc, Mutex};

use chokan_dic::ChokanDictionary;

use crate::user_pref::UserPref;

pub struct MethodContext {
    /// 全体で利用する辞書
    pub dictionary: Arc<Mutex<ChokanDictionary>>,

    /// ユーザーの設定として管理するもの
    pub user_pref: Arc<Mutex<UserPref>>,
}

impl MethodContext {
    /// 新規に[MethodContext]を生成する
    pub fn new(dictionary: Arc<Mutex<ChokanDictionary>>, user_pref: Arc<Mutex<UserPref>>) -> Self {
        MethodContext {
            dictionary: dictionary.clone(),
            user_pref: user_pref.clone(),
        }
    }
}
