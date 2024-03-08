use std::io::{Read, Write};

use crate::base::{
    dictionary,
    io::{DictionaryReader, DictionaryWriter},
};

use super::dic_grammer;

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

pub struct StandardDictionaryReader<R: Read> {
    reader: R,
}

impl<R: Read> StandardDictionaryReader<R> {
    /// 特定のReaderを指定して、新しいStandardDictionaryReaderを生成する
    pub fn new(reader: R) -> Self {
        StandardDictionaryReader { reader }
    }
}

impl<R: Read> DictionaryReader for StandardDictionaryReader<R> {
    fn read_all(&mut self, buf: &mut dictionary::Dictionary) -> Result<usize, std::io::Error> {
        let mut content = String::new();
        self.reader.read_to_string(&mut content)?;
        let lines = content.split('\n').collect::<Vec<_>>();
        let mut entry_count = 0;

        for (index, line) in lines.iter().enumerate() {
            match dic_grammer::parse_entry(line) {
                Ok(Some(entry)) => {
                    buf.add_entry(entry);
                    entry_count += 1;
                }
                Ok(None) => {}
                Err(e) => {
                    eprintln!("Error at line {}: {}", index + 1, e);
                }
            }
        }

        Ok(entry_count)
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
    fn write_and_read_entries() {
        // arrange
        let entry1 = Entry::from_jisyo("おおき", "大き", Speech::Adjective);
        let entry2 = Entry::from_jisyo("ひ", "引", Speech::Verb(VerbForm::Godan("カ".to_string())));

        let dictionary = Dictionary::new(vec![entry1.clone(), entry2.clone()]);
        let file = tempfile::NamedTempFile::with_prefix("chokan").unwrap();
        let another = file.reopen().unwrap();

        // act
        if StandardDictionaryWriter::new(file)
            .write_all(&dictionary)
            .is_err()
        {
            panic!("Failed to write empty dictionary");
        }
        let mut dic = Dictionary::default();
        if StandardDictionaryReader::new(another)
            .read_all(&mut dic)
            .is_err()
        {
            panic!("Failed to write empty dictionary");
        }

        // assert

        let entries = dic.entries_ref();
        assert_eq!(entries.len(), 2);
        assert!(entries.contains(&entry1), "contains entry1");
        assert!(entries.contains(&entry2), "contains entry2");
    }
}
