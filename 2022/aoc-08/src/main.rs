use std::fs;
use std::time::Instant;

type Tree = u32;
type TreeLine = [Tree; 99];
type Forest = [TreeLine; 99];

fn parse(input: &str) -> Forest {
    let mut forest: Forest = [[0; 99]; 99];
    for (y, line) in input.lines().enumerate() {
        for (x, char) in line.chars().enumerate() {
            forest[y][x] = char.to_digit(10).expect("failed to parse char to uint");
        }
    }
    return forest;
}

fn visible_from_top(forest: &Forest, y: usize, x: usize) -> (bool, i32) {
    let tree = forest[y][x];
    let mut count = 0;
    for top in (0..y).into_iter().rev() {
        count += 1;
        if forest[top][x] >= tree {
            return (false, count);
        }
    }
    return (true, count);
}

fn visible_from_bottom(forest: &Forest, y: usize, x: usize) -> (bool, i32) {
    let tree = forest[y][x];
    let mut count = 0;
    for bottom in y + 1..99 {
        count += 1;
        if forest[bottom][x] >= tree {
            return (false, count);
        }
    }
    return (true, count);
}

fn visible_from_left(forest: &Forest, y: usize, x: usize) -> (bool, i32) {
    let tree = forest[y][x];
    let mut count = 0;
    for left in (0..x).into_iter().rev() {
        count += 1;
        if forest[y][left] >= tree {
            return (false, count);
        }
    }
    return (true, count);
}

fn visible_from_right(forest: &Forest, y: usize, x: usize) -> (bool, i32) {
    let tree = forest[y][x];
    let mut count = 0;
    for right in x + 1..99 {
        count += 1;
        if forest[y][right] >= tree {
            return (false, count);
        }
    }
    return (true, count);
}

fn is_visible(forest: &Forest, y: usize, x: usize) -> bool {
    if visible_from_top(forest, y, x).0 {
        return true;
    }
    if visible_from_bottom(forest, y, x).0 {
        return true;
    }
    if visible_from_left(forest, y, x).0 {
        return true;
    }
    if visible_from_right(forest, y, x).0 {
        return true;
    }
    return false;
}

fn solve_part_1(forest: &Forest) -> i32 {
    let mut visible_trees = 0;
    for y in 0..99 {
        for x in 0..99 {
            if is_visible(forest, y, x) {
                visible_trees += 1;
            }
        }
    }
    return visible_trees;
}

fn solve_part_2(forest: &Forest) -> i32 {
    let mut best = 0;
    for y in 0..99 {
        for x in 0..99 {
            let score = visible_from_top(forest, y, x).1
                * visible_from_bottom(forest, y, x).1
                * visible_from_left(forest, y, x).1
                * visible_from_right(forest, y, x).1;
            if score > best {
                best = score;
            }
        }
    }
    return best;
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
fn test_visible() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");

    let forest = parse(&contents);

    assert_eq!(is_visible(&forest, 1, 1), false);
    assert_eq!(is_visible(&forest, 0, 0), true);
    assert_eq!(is_visible(&forest, 3, 3), true);
    assert_eq!(is_visible(&forest, 3, 16), true);
    assert_eq!(is_visible(&forest, 35, 28), true);
}
