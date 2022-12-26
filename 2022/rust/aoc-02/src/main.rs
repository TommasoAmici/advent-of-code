use std::collections::HashMap;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

const WIN: i32 = 6;
const DRAW: i32 = 3;
const LOSS: i32 = 0;

fn part_1(opponent: &str, me: &str) -> i32 {
    let scores_map = HashMap::from([("X", 1), ("Y", 2), ("Z", 3)]);
    let win_map = HashMap::from([("C", "X"), ("B", "Z"), ("A", "Y")]);
    let lose_map = HashMap::from([("C", "Y"), ("B", "X"), ("A", "Z")]);

    let win = match win_map.get(opponent) {
        Some(v) => v == &me,
        _ => false,
    };
    let lose = match lose_map.get(opponent) {
        Some(v) => v == &me,
        _ => false,
    };
    let outcome_score: i32 = if win {
        WIN
    } else if lose {
        LOSS
    } else {
        DRAW
    };

    let choice_score = match scores_map.get(me) {
        Some(s) => *s,
        _ => 0,
    };

    return choice_score + outcome_score;
}

fn part_2(opponent: &str, outcome: &str) -> i32 {
    let choice_to_make_w = HashMap::from([("A", "B"), ("B", "C"), ("C", "A")]);
    let choice_to_make_l = HashMap::from([("A", "C"), ("B", "A"), ("C", "B")]);
    let scores_map = HashMap::from([("A", 1), ("B", 2), ("C", 3)]);
    let result_score_map = HashMap::from([("X", 0), ("Y", 3), ("Z", 6)]);

    let outcome_score = match result_score_map.get(outcome) {
        Some(v) => *v,
        _ => 0,
    };

    let choice_to_make = if outcome_score == WIN {
        match choice_to_make_w.get(opponent) {
            Some(v) => *v,
            _ => panic!("Invalid choice"),
        }
    } else if outcome_score == LOSS {
        match choice_to_make_l.get(opponent) {
            Some(v) => *v,
            _ => panic!("Invalid choice"),
        }
    } else {
        opponent
    };
    let choice_score = match scores_map.get(choice_to_make) {
        Some(v) => *v,
        _ => 0,
    };

    return choice_score + outcome_score;
}

fn solve(file_path: &str) -> io::Result<()> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    let mut total_part1: i32 = 0;
    let mut total_part2: i32 = 0;

    for _line in reader.lines() {
        let line = match _line {
            Ok(l) => l,
            Err(error) => panic!("Can't read line: {:?}", error),
        };

        let choices: Vec<&str> = line.split(" ").collect();
        let opponent = choices[0];
        let me = choices[1];

        total_part1 += part_1(opponent, me);
        total_part2 += part_2(opponent, me);
    }
    println!("Part 1: {}", total_part1);
    println!("Part 2: {}", total_part2);

    Ok(())
}

fn main() {
    let file_path = "./data/input.txt";
    solve(file_path).unwrap();
}
