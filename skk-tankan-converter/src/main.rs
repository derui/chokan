use std::{collections::HashSet, env, error::Error, fs::File, io::Read};

use dic::base::entry::Entry;
use encoding_rs_io::DecodeReaderBytesBuilder;

mod tankan_grammer;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();

    let filepath = &args[1];

    let file = File::open(filepath)?;
    let transcoded = DecodeReaderBytesBuilder::new()
        .encoding(Some(encoding_rs::EUC_JP))
        .build(file);
    let mut content = String::new();
    std::io::BufReader::new(transcoded).read_to_string(&mut content)?;
    let lines = content.split('\n').collect::<Vec<_>>();

    let mut all_entries: HashSet<Entry> = HashSet::new();

    for (index, line) in lines.iter().enumerate() {
        match tankan_grammer::parse_tankan(line) {
            Ok(Some(note)) => {
                let entries = note.to_entries();

                for entry in entries {
                    all_entries.insert(entry);
                }
            }
            Ok(None) => {}
            Err(e) => {
                eprintln!("Error at line {}: {}", index + 1, e);
            }
        }
    }

    for entry in all_entries {
        println!("{}", entry);
    }

    Ok(())
}
