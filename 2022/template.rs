use std::collections::HashSet;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn solve_part_1(lines: Vec<String>) -> i32 {
    // TODO
}

fn solve_part_2(lines: Vec<String>) -> i32 {
    // TODO
}

fn main() {
    let file_path = "./data/input.txt";
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().map(|l| l.unwrap()).collect();

    let part_1 = solve_part_1(lines.clone());
    println!("{}", part_1);

    let part_2 = solve_part_2(lines.clone());
    println!("{}", part_2);
}

#[test]
fn test_solve() {
    let case = vec![
        String::from("XXX"),
    ];
    assert_eq!(solve_part_1(case.clone()), XXX);
    assert_eq!(solve_part_2(case.clone()), XXX);
}
