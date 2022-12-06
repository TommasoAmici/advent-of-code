use std::{collections::HashSet, fs};

fn all_different(chars: [char; 4]) -> bool {
    let set = HashSet::from(chars);
    return set.len() == 4;
}

fn solve_part_1(line: &str) -> i32 {
    let mut chars: [char; 4] = Default::default();
    for (i, c) in line.chars().enumerate() {
        chars[i % 4] = c;

        if i >= 4 {
            if all_different(chars) {
                return (i + 1) as i32;
            }
        }
    }
    return -1;
}

fn all_different_2(chars: [char; 14]) -> bool {
    let set = HashSet::from(chars);
    return set.len() == 14;
}

fn solve_part_2(line: &str) -> i32 {
    let mut chars: [char; 14] = Default::default();
    for (i, c) in line.chars().enumerate() {
        chars[i % 14] = c;

        if i >= 14 {
            if all_different_2(chars) {
                return (i + 1) as i32;
            }
        }
    }
    return -1;
}

fn main() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");

    let part_1 = solve_part_1(&contents);
    println!("{}", part_1);

    let part_2 = solve_part_2(&contents);
    println!("{}", part_2);
}

#[test]
fn test_solve_part_1() {
    for (case, expected) in vec![
        ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
        ("nppdvjthqldpwncqszvftbrmjlhg", 6),
        ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
        ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11),
    ] {
        assert_eq!(solve_part_1(case), expected);
    }
}

#[test]
fn test_solve_part_2() {
    for (case, expected) in vec![
        ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
        ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
        ("nppdvjthqldpwncqszvftbrmjlhg", 23),
        ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
        ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26),
    ] {
        assert_eq!(solve_part_2(case), expected);
    }
}
