use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn read_file(file_path: &String) -> io::Result<()> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);
    for _line in reader.lines() {
        let line = match _line {
            Ok(l) => l,
            Err(error) => panic!("Can't read line: {:?}", error),
        };
        // XXX
    }
    Ok(())
}

fn main() {
    read_file(file_path);
}
