
// 辞書全体を構成する型。
// この型は、辞書ファイルを読み込んで構築され、またimmutableな編集ができる。
#[derive(Debug)]
pub struct Dictionary {
    pub entries: Vec<Entry>,
}

