use std::collections::HashSet;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn find_sections(assignment: &str) -> HashSet<i32> {
    let limits: Vec<&str> = assignment.split("-").collect();
    let start: i32 = limits[0].parse().unwrap();
    let end: i32 = limits[1].parse().unwrap();
    return HashSet::from_iter(start..end + 1);
}

fn find_sets(line: String) -> (HashSet<i32>, HashSet<i32>) {
    let elves: Vec<&str> = line.split(",").collect();
    let fst = find_sections(elves[0]);
    let snd = find_sections(elves[1]);
    return (fst, snd);
}

fn full_overlap(line: String) -> bool {
    let (fst, snd) = find_sets(line);
    return fst.difference(&snd).count() == 0 || snd.difference(&fst).count() == 0;
}

fn partial_overlap(line: String) -> bool {
    let (fst, snd) = find_sets(line);
    return fst.intersection(&snd).count() > 0;
}

fn full_overlap_total(lines: Vec<String>) -> i32 {
    let mut total = 0;
    for line in lines {
        if full_overlap(line) {
            total += 1
        }
    }
    return total;
}

fn partial_overlap_total(lines: Vec<String>) -> i32 {
    let mut total = 0;
    for line in lines {
        if partial_overlap(line) {
            total += 1
        }
    }
    return total;
}

fn main() {
    let file_path = "./data/input.txt";
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().map(|l| l.unwrap()).collect();

    let part_1 = full_overlap_total(lines.clone());
    println!("{}", part_1);

    let part_2 = partial_overlap_total(lines.clone());
    println!("{}", part_2);
}

#[test]
fn test_find_sections() {
    let set = find_sections("6-8");
    for (case, expected) in vec![(&5, false), (&6, true), (&7, true), (&8, true), (&9, false)] {
        assert_eq!(set.contains(case), expected);
    }
}

#[test]
fn test_full_overlap() {
    for (case, expected) in vec![
        (String::from("2-4,6-8"), false),
        (String::from("6-6,4-6"), true),
        (String::from("4-6,6-6"), true),
    ] {
        assert_eq!(full_overlap(case), expected);
    }
}

#[test]
fn test_partial_overlap() {
    for (case, expected) in vec![
        (String::from("2-4,6-8"), false),
        (String::from("6-6,4-6"), true),
        (String::from("4-6,6-6"), true),
        (String::from("2-6,4-8"), true),
    ] {
        assert_eq!(partial_overlap(case), expected);
    }
}

#[test]
fn test_full_overlap_total() {
    let case = vec![
        String::from("2-4,6-8"),
        String::from("2-3,4-5"),
        String::from("5-7,7-9"),
        String::from("2-8,3-7"),
        String::from("6-6,4-6"),
        String::from("2-6,4-8"),
    ];
    let result = full_overlap_total(case);
    assert_eq!(result, 2);
}

#[test]
fn test_partial_overlap_total() {
    let case = vec![
        String::from("2-4,6-8"),
        String::from("2-3,4-5"),
        String::from("5-7,7-9"),
        String::from("2-8,3-7"),
        String::from("6-6,4-6"),
        String::from("2-6,4-8"),
    ];
    let result = partial_overlap_total(case);
    assert_eq!(result, 4);
}
