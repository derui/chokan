use std::io::Write;

use crate::base::{dictionary, io::DictionaryWriter};

/// 標準辞書に対して書き込みを行うためのWriter
pub struct StandardDictionaryWriter<W: Write> {
    writer: W,
}

impl<W: Write> StandardDictionaryWriter<W> {
    /// 特定のWriterを指定して、新しいStandardDictionaryWriterを生成する
    /// ここでは、writerを実装しているものであればなんでも構わない。
    pub fn new(writer: W) -> Self {
        StandardDictionaryWriter { writer }
    }
}

impl<W: Write> DictionaryWriter for StandardDictionaryWriter<W> {
    fn write_all(&mut self, buf: &dictionary::Dictionary) -> Result<usize, std::io::Error> {
        let refs = buf.entries_ref();
        let mut total_size: usize = 0;

        for entry in refs {
            let value = format!("{}\n", entry);
            let size = self.writer.write(value.as_bytes())?;

            total_size += size;
        }
        self.writer.flush()?;

        Ok(total_size)
    }
}

#[cfg(test)]
mod tests {
    use std::io::Read;

    use crate::base::{
        dictionary::Dictionary,
        entry::Entry,
        speech::{Speech, VerbForm},
    };

    use super::*;

    #[test]
    fn write_empty_dictionary() {
        // arrange
        let dictionary = Dictionary::new(vec![]);
        let file = tempfile::NamedTempFile::with_prefix("chokan").unwrap();
        let mut another = file.reopen().unwrap();

        // act

        if StandardDictionaryWriter::new(file)
            .write_all(&dictionary)
            .is_err()
        {
            panic!("Failed to write empty dictionary");
        }

        // assert
        let mut buf = String::new();
        another.read_to_string(&mut buf).unwrap();
        assert_eq!(buf.len(), 0);
    }

    #[test]
    fn write_with_entries() {
        // arrange
        let dictionary = Dictionary::new(vec![
            Entry::new("大き", vec!["い".to_string()], "おおき", Speech::Adjective),
            Entry::new(
                "引",
                vec!["く".to_string()],
                "ひ",
                Speech::Verb(VerbForm::Godan("カ".to_string())),
            ),
        ]);
        let file = tempfile::NamedTempFile::with_prefix("chokan").unwrap();
        let mut another = file.reopen().unwrap();

        // act

        if StandardDictionaryWriter::new(file)
            .write_all(&dictionary)
            .is_err()
        {
            panic!("Failed to write empty dictionary");
        }

        // assert
        let mut buf = String::new();
        another.read_to_string(&mut buf).unwrap();
        assert_eq!(
            buf,
            r#"おおきい	大きい	/形容詞/
ひく	引く	/カ行五段/
"#
        );
    }
}
