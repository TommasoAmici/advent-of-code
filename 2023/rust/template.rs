use std::fs;
use std::time::Instant;

type XXX = i32;

fn parse(input: &str) -> XXX {
    todo!()
}

fn solve_part_1(puzzle: &XXX) -> i32 {
    todo!()
}

fn solve_part_2(puzzle: &XXX) -> i32 {
    todo!()
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
fn test_solve_part_1() {
    let case = "";
    assert_eq!(solve_part_1(case), 0);
}

#[test]
fn test_solve_part_2() {
    let case = "";
    assert_eq!(solve_part_2(case), 0);
}
