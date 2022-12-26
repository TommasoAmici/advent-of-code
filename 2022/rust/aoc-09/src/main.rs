use std::collections::HashSet;
use std::fs;
use std::time::Instant;

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

type RopeMotion = (Direction, u32);

type Knot = (i32, i32);

type Rope = [Knot; 10];

/// A function to parse the puzzle input
fn parse(input: &str) -> Vec<RopeMotion> {
    let mut motions: Vec<RopeMotion> = Vec::new();

    for line in input.lines() {
        let (dir, size) = line
            .split_once(" ")
            .expect("expected direction and motion separated by a space");

        let parsed: u32 = size.parse().expect("expected a number");

        let direction = match dir {
            "U" => Direction::Up,
            "D" => Direction::Down,
            "L" => Direction::Left,
            "R" => Direction::Right,
            other => panic!("Unexpected direction: {}", other),
        };
        motions.push((direction, parsed));
    }

    return motions;
}

/// Returns `true` if the tail should move based on the `head` position.
///
/// The tail should move whenever the head is more than one step away from the tail in any
/// direction.
///
/// ```text
/// ...H...
/// .H...H.
/// .H.T.H.
/// .H...H.
/// ...H...
///```
///
/// Example
/// ```
/// should_move(&(0, 0), &(0, 0)) == false
/// should_move(&(2, 1), &(0, 0)) == true
/// ```
fn should_move(head: &Knot, tail: &Knot) -> bool {
    return !(head.0.abs_diff(tail.0) <= 1 && head.1.abs_diff(tail.1) <= 1);
}

fn move_tail(head: &Knot, tail: &Knot) -> Option<Knot> {
    if !should_move(head, tail) {
        return None;
    }
    if (head.0 - tail.0).abs() == (head.1 - tail.1).abs() {
        return Some(((head.0 + tail.0) / 2, (head.1 + tail.1) / 2));
    } else if (head.0 - tail.0).abs() > (head.1 - tail.1).abs() {
        return Some(((head.0 + tail.0) / 2, head.1));
    } else {
        return Some((head.0, (head.1 + tail.1) / 2));
    }
}

fn solve_part_1(motions: &Vec<RopeMotion>) -> usize {
    let mut head: Knot = (0, 0);
    let mut tail: Knot = (0, 0);
    let mut visited: HashSet<Knot> = HashSet::from([tail.clone()]);

    for motion in motions {
        for _ in 0..motion.1 {
            match motion.0 {
                Direction::Up => {
                    head.1 += 1;
                }
                Direction::Down => {
                    head.1 -= 1;
                }
                Direction::Left => {
                    head.0 -= 1;
                }
                Direction::Right => {
                    head.0 += 1;
                }
            };
            match move_tail(&head, &tail) {
                Some(t) => {
                    tail = t;
                    visited.insert(t);
                }
                None => continue,
            };
        }
    }

    return visited.len();
}

fn solve_part_2(motions: &Vec<RopeMotion>) -> usize {
    let mut rope: Rope = [(0, 0); 10];
    let mut visited: HashSet<Knot> = HashSet::new();

    for motion in motions {
        for _ in 0..motion.1 {
            match motion.0 {
                Direction::Up => {
                    rope[0].1 += 1;
                }
                Direction::Down => {
                    rope[0].1 -= 1;
                }
                Direction::Left => {
                    rope[0].0 -= 1;
                }
                Direction::Right => {
                    rope[0].0 += 1;
                }
            };
            for i in 1..rope.len() {
                match move_tail(&rope[i - 1], &rope[i]) {
                    Some(t) => rope[i] = t,
                    None => continue,
                }
            }
            visited.insert(rope[9]);
        }
    }

    return visited.len();
}

fn main() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");

    let parse_start = Instant::now();
    let motions = parse(&contents);
    println!("parsing: {}µs", parse_start.elapsed().as_micros());

    let part_1_start = Instant::now();
    let part_1 = solve_part_1(&motions);
    println!("{} {}µs", part_1, part_1_start.elapsed().as_micros());

    let part_2_start = Instant::now();
    let part_2 = solve_part_2(&motions);
    println!("{} {}µs", part_2, part_2_start.elapsed().as_micros());
}

#[test]
fn test_solve_part_1() {
    let motions = parse(
        "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2",
    );
    assert_eq!(solve_part_1(&motions), 13);
}

#[test]
fn test_solve_part_2() {
    let motions = parse(
        "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20",
    );
    assert_eq!(solve_part_2(&motions), 36);
}

#[test]
fn test_should_move() {
    assert_eq!(should_move(&(1, 0), &(0, 0)), false);
    assert_eq!(should_move(&(1, 1), &(0, 0)), false);
    assert_eq!(should_move(&(0, 1), &(0, 0)), false);
    assert_eq!(should_move(&(-1, 1), &(0, 0)), false);
    assert_eq!(should_move(&(-1, 0), &(0, 0)), false);
    assert_eq!(should_move(&(-1, -1), &(0, 0)), false);
    assert_eq!(should_move(&(0, -1), &(0, 0)), false);
    assert_eq!(should_move(&(1, -1), &(0, 0)), false);

    assert_eq!(should_move(&(2, 0), &(0, 0)), true);
    assert_eq!(should_move(&(2, 1), &(0, 0)), true);
    assert_eq!(should_move(&(2, 2), &(0, 0)), true);
    assert_eq!(should_move(&(1, 2), &(0, 0)), true);
    assert_eq!(should_move(&(0, 2), &(0, 0)), true);
    assert_eq!(should_move(&(-1, 2), &(0, 0)), true);
    assert_eq!(should_move(&(-2, 2), &(0, 0)), true);
    assert_eq!(should_move(&(-2, 1), &(0, 0)), true);
    assert_eq!(should_move(&(-2, 0), &(0, 0)), true);
    assert_eq!(should_move(&(-2, -1), &(0, 0)), true);
    assert_eq!(should_move(&(-2, -2), &(0, 0)), true);
    assert_eq!(should_move(&(-1, -2), &(0, 0)), true);
    assert_eq!(should_move(&(0, -2), &(0, 0)), true);
    assert_eq!(should_move(&(1, -2), &(0, 0)), true);
    assert_eq!(should_move(&(2, -2), &(0, 0)), true);
    assert_eq!(should_move(&(2, -1), &(0, 0)), true);
}

#[test]
fn test_move_tail() {
    assert_eq!(move_tail(&(1, 0), &(0, 0)), None);
    assert_eq!(move_tail(&(1, 1), &(0, 0)), None);
    assert_eq!(move_tail(&(0, 1), &(0, 0)), None);
    assert_eq!(move_tail(&(-1, 1), &(0, 0)), None);
    assert_eq!(move_tail(&(-1, 0), &(0, 0)), None);
    assert_eq!(move_tail(&(-1, -1), &(0, 0)), None);
    assert_eq!(move_tail(&(0, -1), &(0, 0)), None);
    assert_eq!(move_tail(&(1, -1), &(0, 0)), None);

    assert_eq!(move_tail(&(2, 0), &(0, 0)), Some((1, 0)));
    assert_eq!(move_tail(&(2, 1), &(0, 0)), Some((1, 1)));
    assert_eq!(move_tail(&(2, 2), &(0, 0)), Some((1, 1)));
    assert_eq!(move_tail(&(1, 2), &(0, 0)), Some((1, 1)));
    assert_eq!(move_tail(&(0, 2), &(0, 0)), Some((0, 1)));
    assert_eq!(move_tail(&(-1, 2), &(0, 0)), Some((-1, 1)));
    assert_eq!(move_tail(&(-2, 2), &(0, 0)), Some((-1, 1)));
    assert_eq!(move_tail(&(-2, 1), &(0, 0)), Some((-1, 1)));
    assert_eq!(move_tail(&(-2, 0), &(0, 0)), Some((-1, 0)));
    assert_eq!(move_tail(&(-2, -1), &(0, 0)), Some((-1, -1)));
    assert_eq!(move_tail(&(-2, -2), &(0, 0)), Some((-1, -1)));
    assert_eq!(move_tail(&(-1, -2), &(0, 0)), Some((-1, -1)));
    assert_eq!(move_tail(&(0, -2), &(0, 0)), Some((0, -1)));
    assert_eq!(move_tail(&(1, -2), &(0, 0)), Some((1, -1)));
    assert_eq!(move_tail(&(2, -2), &(0, 0)), Some((1, -1)));
    assert_eq!(move_tail(&(2, -1), &(0, 0)), Some((1, -1)));
}
