use std::fs;
use std::iter;

type Crate = char;
type Stack = Vec<Crate>;
type Move = usize;
type From = usize;
type To = usize;
type Moves = (Move, From, To);

#[derive(Debug, Clone)]
struct Puzzle {
    pub stacks: Vec<Stack>,
    pub moves: Vec<Moves>,
}

fn find_max_stacks(source: &str) -> usize {
    match source.lines().map(|f| f.len()).max() {
        Some(max) => max / 4,
        None => panic!("Expected stacks to exist"),
    }
}

fn parse_stacks(source: &str) -> Vec<Stack> {
    let max_stacks = find_max_stacks(source);
    println!("{}", max_stacks);
    let mut stacks: Vec<Stack> = iter::repeat_with(|| vec![]).take(max_stacks + 1).collect();
    for line in source.lines() {
        if !line.starts_with("[") {
            continue;
        }
        for (i, char) in line.chars().enumerate() {
            if i % 4 == 1 {
                // println!("{}:{}: {}", i / 4, 1 + i / 4, char);
                if char != ' ' {
                    stacks[i / 4].push(char);
                }
            }
        }
    }
    let reversed_stacks: Vec<Stack> = stacks
        .into_iter()
        .map(|stack| stack.into_iter().rev().collect())
        .collect();
    return reversed_stacks;
}

fn parse_moves(source: &str) -> Vec<Moves> {
    let mut moves: Vec<Moves> = Vec::new();
    for line in source.lines() {
        let mut m: Move = 0;
        let mut f: From = 0;
        let mut t: To = 0;
        for (i, l) in line.split_whitespace().enumerate() {
            if i == 1 {
                m = l.parse().unwrap();
            } else if i == 3 {
                f = l.parse().unwrap();
            } else if i == 5 {
                t = l.parse().unwrap();
            }
        }
        moves.push((m, f - 1, t - 1));
    }
    return moves;
}

fn parse_input(contents: String) -> Puzzle {
    let (stacks_source, moves_source) = contents
        .split_once("\n\n")
        .expect("Couldn't find two parts");

    let puzzle: Puzzle = Puzzle {
        stacks: parse_stacks(stacks_source),
        moves: parse_moves(moves_source),
    };

    return puzzle;
}

fn get_top_crates(puzzle: Puzzle) -> String {
    let top_crates: String = puzzle
        .stacks
        .into_iter()
        .map(|s| *s.last().unwrap())
        .collect();

    return top_crates;
}

fn solve_part_1(mut puzzle: Puzzle) -> String {
    for m in puzzle.moves.iter() {
        let origin_index = m.1;
        let destination_index = m.2;
        for _ in 0..m.0 {
            match puzzle.stacks[origin_index].pop() {
                Some(x) => {
                    puzzle.stacks[destination_index].push(x);
                }
                None => continue,
            }
        }
    }
    return get_top_crates(puzzle);
}

fn solve_part_2(mut puzzle: Puzzle) -> String {
    for m in puzzle.moves.iter() {
        let origin_index = m.1;
        let destination_index = m.2;
        let final_length = puzzle.stacks[origin_index].len() - m.0;
        let mut tail = puzzle.stacks[origin_index].split_off(final_length);
        puzzle.stacks[destination_index].append(&mut tail);
    }
    return get_top_crates(puzzle);
}

fn main() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");
    let puzzle = parse_input(contents);
    let part_1 = solve_part_1(puzzle.clone());
    println!("{}", part_1);

    let part_2 = solve_part_2(puzzle.clone());
    println!("{}", part_2);
}
