use std::{
    env,
    error::Error,
    fs::File,
    io::{Read, Write},
    path::Path,
};

use converter::ConvertedEntry;
use encoding_rs_io::DecodeReaderBytesBuilder;

mod converter;
mod note_grammer;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn output_note(note_f: &mut File, entry: &ConvertedEntry) {
    if !entry.is_ancillary() {
        writeln!(note_f, "{}", entry).unwrap();
    }
}

fn try_main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();

    let filepath = &args[1];
    let note_path = Path::new(&args[2]);
    let mut note_file = File::create(note_path)?;

    let file = File::open(filepath)?;
    let transcoded = DecodeReaderBytesBuilder::new()
        .encoding(Some(encoding_rs::EUC_JP))
        .build(file);
    let mut content = String::new();
    std::io::BufReader::new(transcoded).read_to_string(&mut content)?;
    let lines = content.split('\n').collect::<Vec<_>>();

    for (index, line) in lines.iter().enumerate() {
        match note_grammer::parse_note(line) {
            Ok(Some(note)) => {
                let entries = note.to_entries();

                for entry in entries {
                    output_note(&mut note_file, &entry)
                }
            }
            Ok(None) => {}
            Err(e) => {
                eprintln!("Error at line {}: {}", index + 1, e);
            }
        }
    }

    Ok(())
}
