use core::panic;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn part_1(file_path: &str) -> io::Result<()> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    let mut priority_map: HashMap<char, usize> = HashMap::new();
    let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    for (i, c) in alphabet.chars().enumerate() {
        priority_map.insert(c, i + 1);
    }

    let mut priority = 0;

    for _line in reader.lines() {
        let line = match _line {
            Ok(l) => l,
            Err(error) => panic!("Can't read line: {:?}", error),
        };
        let (compartment_1, compartment_2) = line.split_at(line.len() / 2);
        let compartment_1_items: HashSet<char> = HashSet::from_iter(compartment_1.chars());
        for c in compartment_2.chars() {
            match compartment_1_items.get(&c) {
                Some(_) => match priority_map.get(&c) {
                    Some(v) => {
                        priority += v;
                        break;
                    }
                    None => panic!("Priority missing for char {}", c),
                },
                None => continue,
            }
        }
    }
    println!("{}", priority);

    Ok(())
}

fn part_2(file_path: &str) -> io::Result<()> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    let mut priority_map: HashMap<char, usize> = HashMap::new();
    let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    for (i, c) in alphabet.chars().enumerate() {
        priority_map.insert(c, i + 1);
    }

    let mut priority = 0;
    let mut index = 0;
    let mut elves_group: [HashSet<char>; 3] = [HashSet::new(), HashSet::new(), HashSet::new()];

    for _line in reader.lines() {
        let line = match _line {
            Ok(l) => l,
            Err(error) => panic!("Can't read line: {:?}", error),
        };
        elves_group[index % 3] = HashSet::from_iter(line.chars());
        index += 1;
        if index % 3 == 0 {
            let mut common = elves_group[0]
                .iter()
                .filter(|k| elves_group[1].contains(k))
                .filter(|k| elves_group[2].contains(k));
            match common.next() {
                Some(c) => match priority_map.get(&c) {
                    Some(v) => {
                        priority += v;
                    }
                    None => panic!("Priority missing for char {}", c),
                },
                None => panic!("Expected a character"),
            }
        }
    }
    println!("{}", priority);

    Ok(())
}

fn main() {
    let file_path = "./data/input.txt";
    part_1(file_path).unwrap();
    part_2(file_path).unwrap();
}
