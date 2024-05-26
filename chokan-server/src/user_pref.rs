use std::{
    fs,
    io::{Read, Write},
    path::Path,
};

use dic::{
    base::{
        dictionary::Dictionary,
        entry::Entry,
        io::{DictionaryReader, DictionaryWriter},
        speech::Speech,
    },
    standard::io::{StandardDictionaryReader, StandardDictionaryWriter},
};
use kkc::{context::Context, frequency::ConversionFrequency, Candidate};

const USER_DICTIONARY_NAME: &str = "user.dic";
const USER_FREQUENCY_NAME: &str = "frequency.bin";

/// ユーザーの更新によって変換されうる辞書の状態を保持するための構造体
pub struct UserPref {
    /// ユーザーごとの変換頻度
    frequency: ConversionFrequency,

    /// ユーザー辞書。この形式のままファイルへの書き込みを行うので、全体標準の形式を利用している。
    user_dictionary: Dictionary,

    /// 保存先のディレクトリ。
    user_directory_dir: Option<Box<Path>>,
}

impl UserPref {
    /// 新規に[MethodContext]を生成する
    pub fn new(
        frequency: ConversionFrequency,
        user_dictionary: Dictionary,
        user_directory_dir: Option<Box<Path>>,
    ) -> Self {
        UserPref {
            frequency,
            user_dictionary,
            user_directory_dir,
        }
    }

    /// [ConversionFrequency::update_word]のラッパー
    ///
    /// # Arguments
    /// * `word` - 更新する単語
    /// * `context` - コンテキスト
    pub fn update_frequency(&mut self, word: &str, context: &Context) {
        self.frequency.update_word(word, context);
    }

    /// ユーザー定義辞書に複合語を登録する
    ///
    /// ここでの複合語は、接辞が含まれるもののみに限っており、一般的に言う複合語とは異なる。
    ///
    /// # Arguments
    /// * `candidate` - 対象のcandidate
    pub fn update_compound_words(&mut self, candidate: &Candidate) -> Option<Entry> {
        candidate
            .to_string_with_affix()
            .map(|(word, reading)| {
                Entry::from_jisyo(
                    &reading,
                    &word,
                    Speech::Noun(dic::base::speech::NounVariant::Common),
                )
            })
            .inspect(|entry| {
                self.user_dictionary.add_entry(entry.clone());
                tracing::info!("Learned new entry: {}", entry);
            })
    }

    pub fn frequency(&self) -> &ConversionFrequency {
        &self.frequency
    }

    pub fn user_dictionary(&self) -> &Dictionary {
        &self.user_dictionary
    }

    pub fn user_dictionary_mut(&mut self) -> &mut Dictionary {
        &mut self.user_dictionary
    }

    /// ユーザーごとの設定を復元する
    ///
    /// # Arguments
    /// - `user_directory` * ユーザー情報を投入するdirectory
    ///
    /// # Returns
    /// 読み出した[UserPref]。ディレクトリが存在しなかったり、ファイルが存在しない場合はデフォルトが返却される
    pub fn restore_user_pref(user_directory: Box<Path>) -> anyhow::Result<UserPref> {
        match user_directory.try_exists() {
            Ok(v) if v => {
                let path = user_directory.join(USER_FREQUENCY_NAME);
                let frequency: ConversionFrequency = if path.exists() {
                    let mut file = std::fs::File::open(path.clone())?;
                    let mut bytes = vec![];
                    file.read_to_end(&mut bytes)?;
                    let v = postcard::from_bytes(&bytes)?;
                    tracing::info!(
                        "Restored user frequency information from {}",
                        path.display()
                    );
                    v
                } else {
                    ConversionFrequency::new()
                };

                let path = user_directory.join(USER_DICTIONARY_NAME);
                let user_dictionary = if path.exists() {
                    let file = std::fs::File::open(path.clone())?;

                    let mut reader = StandardDictionaryReader::new(file);
                    let mut user_dictionary = Dictionary::new(vec![]);
                    reader.read_all(&mut user_dictionary)?;
                    tracing::info!("Restored user dictionary from {}", path.display());
                    user_dictionary
                } else {
                    Dictionary::new(vec![])
                };

                Ok(UserPref::new(
                    frequency,
                    user_dictionary,
                    Some(user_directory.clone()),
                ))
            }
            Ok(_) => Ok(UserPref::new(
                ConversionFrequency::new(),
                Dictionary::new(vec![]),
                Some(user_directory.clone()),
            )),
            Err(e) => Err(anyhow::anyhow!("Failed to create directory: {}", e)),
        }
    }

    /// ユーザー辞書と頻度を保存する
    ///
    /// # Returns
    /// 保存に成功した場合は`Ok(())`を返す
    pub fn save_user_dictionary(&self) -> anyhow::Result<()> {
        if let Some(dir) = &self.user_directory_dir {
            match dir.try_exists() {
                Ok(v) if !v => {
                    fs::create_dir_all(dir)?;
                }
                Ok(_) => (),
                Err(e) => {
                    return Err(anyhow::anyhow!("Failed to create directory: {}", e));
                }
            }
            let path = dir.join(USER_FREQUENCY_NAME);
            let mut file = std::fs::File::create(path)?;
            let bytes = postcard::to_allocvec(&self.frequency)?;
            file.write_all(&bytes)?;

            let path = dir.join(USER_DICTIONARY_NAME);
            let file = std::fs::File::create(path)?;

            let mut writer = StandardDictionaryWriter::new(file);
            writer.write_all(&self.user_dictionary)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn restore_default_if_directory_is_not_found() {
        // arrange

        // act
        let ret =
            UserPref::restore_user_pref(PathBuf::from("/tmp/user-pref").into_boxed_path()).unwrap();

        // assert
        assert_eq!(ret.frequency, ConversionFrequency::new());
        assert_eq!(ret.user_dictionary, Dictionary::new(vec![]));
        assert_eq!(
            ret.user_directory_dir,
            Some(PathBuf::from("/tmp/user-pref").into_boxed_path())
        );
    }
}
