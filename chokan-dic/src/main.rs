use std::{collections::HashMap, env, fs::File, io::Write, path::Path};

use analytics_graph::GraphDictionary;
use dic::{
    base::{dictionary::Dictionary, io::DictionaryReader, word::Word},
    standard::io::StandardDictionaryReader,
};
use log::info;
use postcard::{to_allocvec, to_vec};
use serde::Serialize;

// 一時的に利用するdictionary
struct ReadDictionary {
    trie: trie::Trie,
    dic: HashMap<String, Vec<Word>>,
}

// trie用のキー
const JP_KEYS: &str = "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをんがぎぐげござじずぜぞだぢづでどばびぶべぼぱぴぷぺぽ";
const EN_KEYS: &str = "abcdefghijklmnopqrstuvwxyz";

fn label_keys() -> Vec<char> {
    format!("{}{}", JP_KEYS, EN_KEYS)
        .chars()
        .collect::<Vec<_>>()
}

/// 辞書ファイルを読み込んで、trieと辞書を作成する
fn read_and_make_dictionary(dic_path: &Path) -> Result<ReadDictionary, std::io::Error> {
    let file = File::open(dic_path)?;
    let mut reader = StandardDictionaryReader::new(file);

    let mut dic = Dictionary::default();
    reader.read_all(&mut dic)?;

    let mut trie = trie::Trie::from_keys(&label_keys());
    let mut dic_map = HashMap::new();
    let mut count = 0u64;

    for entry in dic.entries_ref() {
        let words: Vec<Word> = entry.clone().into();

        for word in words {
            count += 1;
            if count % 1000 == 0 {
                info!("Words: {} processed...", count);
            }
            let _ = trie.insert(&word.reading);
            let v = dic_map.entry(word.reading.clone()).or_insert(Vec::new());
            v.push(word);
        }
    }

    Ok(ReadDictionary { trie, dic: dic_map })
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 4 {
        println!(
            "Usage: {} <dictionary file> <huzoku file> <output>",
            args[0]
        );
        return;
    }

    let dic_path = Path::new(&args[1]);
    let huzoku_path = Path::new(&args[2]);
    let output_path = Path::new(&args[3]);

    let standard = read_and_make_dictionary(dic_path).unwrap();
    let huzoku = read_and_make_dictionary(huzoku_path).unwrap();

    let dic = GraphDictionary {
        standard_trie: standard.trie,
        standard_dic: standard.dic,
        ancillary_trie: huzoku.trie,
        ancillary_dic: huzoku.dic,
    };
    let mut output = File::create(output_path).unwrap();
    let bin = to_allocvec(&dic).unwrap();
    output.write_all(&bin).unwrap();
}
