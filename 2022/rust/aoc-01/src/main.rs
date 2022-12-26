use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn find_top_3(elves: Vec<Vec<i64>>) -> Vec<i64> {
    let mut calories: Vec<i64> = Vec::new();
    for elf in elves {
        let c: i64 = elf.iter().sum();
        calories.push(c);
    }
    calories.sort_unstable();
    calories.reverse();
    return calories[0..3].to_vec();
}

fn group_elves(reader: BufReader<File>) -> Vec<Vec<i64>> {
    let mut elves: Vec<Vec<i64>> = Vec::new();
    let mut curr_elf: Vec<i64> = Vec::new();

    for _line in reader.lines() {
        let line = match _line {
            Ok(l) => l,
            Err(error) => panic!("Can't read line: {:?}", error),
        };
        if line != "" {
            let calories = match line.parse::<i64>() {
                Ok(v) => v,
                Err(error) => panic!("Failed to parse integer: {:?}", error),
            };
            curr_elf.push(calories);
        } else {
            elves.push(curr_elf);
            curr_elf = Vec::new();
        }
    }
    if curr_elf.len() > 0 {
        elves.push(curr_elf);
    }
    return elves;
}

fn solve(file_path: &str) -> io::Result<()> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);
    let elves = group_elves(reader);
    let top_3 = find_top_3(elves);
    let biggest = top_3[0];
    println!("Part 1: {}", biggest);
    let top_3_sum: i64 = top_3.iter().sum();
    println!("Part 2: {}", top_3_sum);
    Ok(())
}

fn main() {
    let file_path = "./data/input.txt";
    solve(file_path).unwrap();
}
