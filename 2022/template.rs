use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn solve(file_path: &str) -> io::Result<()> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    for _line in reader.lines() {
        let line = match _line {
            Ok(l) => l,
            Err(error) => panic!("Can't read line: {:?}", error),
        };
        println!("{}", line);
    }

    Ok(())
}

fn main() {
    let file_path = "./data/input.txt";
    solve(file_path).unwrap();
}
