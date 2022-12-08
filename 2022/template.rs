use std::fs;
use std::time::Instant;

fn solve_part_1(input: &str) -> i32 {
    // TODO
    return 0;
}

fn solve_part_2(input: &str) -> i32 {
    // TODO
    return 0;
}

fn main() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");

    let part_1_start = Instant::now();
    let part_1 = solve_part_1(&contents);
    println!("{} {}µs", part_1, part_1_start.elapsed().as_micros());

    let part_2_start = Instant::now();
    let part_2 = solve_part_2(&contents);
    println!("{} {}µs", part_2, part_2_start.elapsed().as_micros());
}

#[test]
fn test_solve_part_1() {
    let case = "";
    assert_eq!(solve_part_1(case), 0);
}

#[test]
fn test_solve_part_2() {
    let case = "";
    assert_eq!(solve_part_2(case), 0);
}
