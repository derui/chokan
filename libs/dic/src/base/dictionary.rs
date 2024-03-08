use super::entry::Entry;

/// 辞書全体を構成する型。
/// この型は、辞書ファイルを読み込んで構築され、またentryの追加、削除が実施できる
/// この辞書においては、同一の品詞などを追加することも可能になっている。ただしあくまで
#[derive(Debug)]
pub struct Dictionary {
    entries: Vec<Entry>,
}

/// [Dictionary]のデフォルトの値を生成する
impl Default for Dictionary {
    fn default() -> Dictionary {
        Dictionary::new(vec![])
    }
}

impl Dictionary {
    /// [Dictionary]を生成する
    pub fn new(entries: Vec<Entry>) -> Dictionary {
        Dictionary { entries }
    }

    /// エントリ全体をcopyして返す
    pub fn entries(&self) -> Vec<Entry> {
        self.entries.to_vec()
    }

    /// エントリ全体のリファレンスを返す
    pub fn entries_ref(&self) -> &Vec<Entry> {
        &self.entries
    }

    /// [Dictionary]に[Entry]を追加する
    pub fn add_entry(&mut self, entry: Entry) {
        self.entries.push(entry);
    }
}
