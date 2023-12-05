use std::collections::{HashMap, HashSet};
use std::fs;
use std::time::Instant;

type Numbers = HashSet<u32>;

#[derive(Debug)]
struct Scratchcard {
    winning: Numbers,
    owned: Numbers,
}

impl Scratchcard {
    fn winning_numbers_count(&self) -> u32 {
        self.winning.intersection(&self.owned).count() as u32
    }

    fn winning_value(&self) -> u32 {
        let winning = self.winning_numbers_count();
        match winning {
            0 => 0,
            1 => 1,
            2 => 2,
            3 => 4,
            4 => 8,
            5 => 16,
            6 => 32,
            7 => 64,
            8 => 128,
            9 => 256,
            10 => 512,
            x => panic!("We won more than {}!", x),
        }
    }
}

fn parse_nums(raw: &str) -> Numbers {
    let vect: Vec<u32> = raw
        .split_whitespace()
        .into_iter()
        .map(|x| x.parse::<u32>().unwrap())
        .collect();
    HashSet::from_iter(vect)
}

fn parse(input: &str) -> Vec<Scratchcard> {
    input
        .lines()
        .map(|line| {
            let (winning_raw, owned_raw) = line.split_once("|").unwrap();
            let (_, winning_nums) = winning_raw.split_once(":").unwrap();
            let winning = parse_nums(winning_nums);
            let owned = parse_nums(owned_raw);

            Scratchcard { winning, owned }
        })
        .collect()
}

fn solve_part_1(puzzle: &Vec<Scratchcard>) -> u32 {
    puzzle.iter().map(|scratch| scratch.winning_value()).sum()
}

fn solve_part_2(puzzle: &Vec<Scratchcard>) -> u32 {
    let mut cards: HashMap<usize, u32> = HashMap::new();
    for (index, card) in puzzle.iter().enumerate() {
        *cards.entry(index).or_default() += 1;
        let count = card.winning_numbers_count();
        for i in index + 1..(index + (count as usize) + 1) {
            *cards.entry(i).or_default() += cards[&index];
        }
    }
    let mut total = 0;
    for (_, t) in cards {
        total += t;
    }
    total
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
fn test_solve_part_2() {
    let case = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
    assert_eq!(solve_part_2(&parse(case)), 30);
}
