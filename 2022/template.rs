use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn todo(reader: BufReader<File>) {
    for _line in reader.lines() {
        let line = match _line {
            Ok(l) => l,
            Err(error) => panic!("Can't read line: {:?}", error),
        };
        println!("{}", line);
    }
}

fn solve(file_path: &str) -> io::Result<()> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);
    todo(reader);
    Ok(())
}

fn main() {
    let file_path = "./data/input.txt";
    solve(file_path).unwrap();
}
