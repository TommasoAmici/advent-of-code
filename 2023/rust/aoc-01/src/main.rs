use std::fs;
use std::time::Instant;

fn parse(input: &str) -> Vec<&str> {
    input.split("\n").collect()
}

fn find_calibration_value(reading: &str) -> u32 {
    if reading == "" {
        return 0;
    }

    let first = reading.find(char::is_numeric).unwrap();
    let last = reading.rfind(char::is_numeric).unwrap();
    let s = format!(
        "{}{}",
        reading.chars().nth(first).unwrap(),
        reading.chars().nth(last).unwrap()
    );
    s.parse::<u32>().unwrap()
}

fn find_patterns(patterns: &Vec<(&str, char)>, reading: &str) -> Option<(usize, char)> {
    let mut value: char = '0';
    let mut index = reading.len();
    for (pat, v) in patterns {
        let i = reading.find(pat);
        match i {
            Some(i) => {
                if i < index {
                    index = i;
                    value = v.clone();
                }
            }
            None => continue,
        }
    }
    match value {
        '0' => None,
        _ => Some((index, value)),
    }
}

fn rfind_patterns(patterns: &Vec<(&str, char)>, reading: &str) -> Option<(usize, char)> {
    let mut value: char = '0';
    let mut index = 0;
    for (pat, v) in patterns {
        let i = reading.rfind(pat);
        match i {
            Some(i) => {
                if i > index {
                    index = i;
                    value = v.clone();
                }
            }
            None => continue,
        }
    }
    match value {
        '0' => None,
        _ => Some((index, value)),
    }
}

fn find_calibration_value_letters(reading: &str) -> u32 {
    if reading == "" {
        return 0;
    }

    let patterns = vec![
        ("one", '1'),
        ("two", '2'),
        ("three", '3'),
        ("four", '4'),
        ("five", '5'),
        ("six", '6'),
        ("seven", '7'),
        ("eight", '8'),
        ("nine", '9'),
    ];

    let first_numeric_index = reading.find(char::is_numeric).unwrap_or(reading.len());
    let first_spelled = find_patterns(&patterns, reading);
    let first = match first_spelled {
        Some((index, value)) => {
            if index < first_numeric_index {
                value
            } else {
                reading.chars().nth(first_numeric_index).unwrap()
            }
        }
        None => reading.chars().nth(first_numeric_index).unwrap(),
    };
    let last_numeric_index = reading.rfind(char::is_numeric).unwrap_or(0);
    let last_spelled = rfind_patterns(&patterns, reading);
    let last = match last_spelled {
        Some((index, value)) => {
            if index > last_numeric_index {
                value
            } else {
                reading.chars().nth(last_numeric_index).unwrap()
            }
        }
        None => reading.chars().nth(last_numeric_index).unwrap(),
    };
    let s = format!("{}{}", first, last);
    s.parse::<u32>().unwrap()
}

fn solve_part_1(puzzle: &Vec<&str>) -> u32 {
    puzzle
        .iter()
        .fold(0, |acc, e| acc + find_calibration_value(e))
}

fn solve_part_2(puzzle: &Vec<&str>) -> u32 {
    puzzle
        .iter()
        .fold(0, |acc, e| acc + find_calibration_value_letters(e))
}

fn main() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");

    let parse_start = Instant::now();
    let puzzle = parse(&contents);
    println!("parsing: {}µs", parse_start.elapsed().as_micros());

    let part_1_start = Instant::now();
    let part_1 = solve_part_1(&puzzle);
    println!("{} {}µs", part_1, part_1_start.elapsed().as_micros());

    let part_2_start = Instant::now();
    let part_2 = solve_part_2(&puzzle);
    println!("{} {}µs", part_2, part_2_start.elapsed().as_micros());
}

#[test]
fn test_find_calibration_value() {
    assert_eq!(find_calibration_value("1abc2"), 12);
    assert_eq!(find_calibration_value("pqr3stu8vwx"), 38);
    assert_eq!(find_calibration_value("a1b2c3d4e5f"), 15);
    assert_eq!(find_calibration_value("treb7uchet"), 77);
}

#[test]
fn test_solve_part_1() {
    assert_eq!(
        solve_part_1(&vec!["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet",]),
        142
    );
}

#[test]
fn test_find_calibration_value_letters() {
    assert_eq!(find_calibration_value_letters("two1nine"), 29);
    assert_eq!(find_calibration_value_letters("eightwothree"), 83);
    assert_eq!(find_calibration_value_letters("abcone2threexyz"), 13);
    assert_eq!(find_calibration_value_letters("xtwone3four"), 24);
    assert_eq!(find_calibration_value_letters("4nineeightseven2"), 42);
    assert_eq!(find_calibration_value_letters("zoneight234"), 14);
    assert_eq!(find_calibration_value_letters("7pqrstsixteen"), 76);
}

#[test]
fn test_solve_part_2() {
    assert_eq!(
        solve_part_2(&vec![
            "two1nine",
            "eightwothree",
            "abcone2threexyz",
            "xtwone3four",
            "4nineeightseven2",
            "zoneight234",
            "7pqrstsixteen"
        ]),
        281
    );
}
