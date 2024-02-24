use std::{env, error::Error, fs::File, io::Read};

use encoding_rs_io::DecodeReaderBytesBuilder;

mod note_grammer;

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
    let lines = content.split("\n").collect::<Vec<_>>();

    for (index, line) in lines.iter().enumerate() {
        println!("{}: {}", index, line);
        match note_grammer::parse_note(*line) {
            Ok(note) => {
                println!("{:?}", note);
            }
            Err(e) => {
                println!("Error at line {}: {}", index, e);
            }
        }
    }

    Ok(())
}
