use std::io;

use super::dictionary::Dictionary;

/// Dictionaryを取得するためのtrait
trait DictonaryReader {
    /// 指定された [Dictionary] に対して、読み込んだエントリを追加する。完了したら、読み込んだエントリ数を返す。
    fn read_all(&self, buf: &mut Dictionary) -> Result<usize, io::Error>;
}

/// Dictionaryをどこかに書き込むためのtrait
trait DictionaryWriter {
    /// 指定された [Dictionary] を特定の場所に一括で書き込む。
    fn write_all(&self, buf: &Dictionary) -> Result<usize, io::Error>;
}
