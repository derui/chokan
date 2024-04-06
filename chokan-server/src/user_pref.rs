use std::{fs, io::Write, path::Path};

use dic::{base::io::DictionaryWriter, standard::io::StandardDictionaryWriter};
use kkc::{context::Context, frequency::ConversionFrequency};

const USER_DICTIONARY_NAME: &str = "user.dic";
const USER_FREQUENCY_NAME: &str = "frequency.bin";

/// ユーザーの更新によって変換されうる辞書の状態を保持するための構造体
pub struct UserPref {
    /// ユーザー辞書。この形式のままファイルへの書き込みを行うので、全体標準の形式を利用している。
    frequency: ConversionFrequency,

    /// ユーザー辞書。この形式のままファイルへの書き込みを行うので、全体標準の形式を利用している。
    user_dictionary: dic::base::dictionary::Dictionary,

    /// 保存先のディレクトリ。
    user_directory_dir: Option<Box<Path>>,
}

impl UserPref {
    /// 新規に[MethodContext]を生成する
    pub fn new(
        frequency: ConversionFrequency,
        user_dictionary: dic::base::dictionary::Dictionary,
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

    pub fn frequency(&self) -> &ConversionFrequency {
        &self.frequency
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
