use std::fs;

use std::time::Instant;

#[derive(Default, Debug)]
struct Set {
    red: u32,
    green: u32,
    blue: u32,
}

#[derive(Debug)]
struct Game {
    id: u32,
    sets: Vec<Set>,
}

fn parse(input: &str) -> Vec<Game> {
    let mut games = Vec::new();
    for line in input.split("\n") {
        if line.is_empty() {
            continue;
        }

        let mut sets = Vec::new();
        let (game_id, game_sets) = line.split_once(":").unwrap();
        let (_prefix, id) = game_id.split_once(" ").unwrap();
        let id = id.parse::<u32>().unwrap();
        for raw_set in game_sets.split(";") {
            let mut set = Set::default();
            let cubes = raw_set.split(",");
            for cube in cubes {
                let (num, color) = cube.trim().split_once(" ").unwrap();
                let parsed_num = num.parse::<u32>().unwrap();
                match color {
                    "red" => set.red = parsed_num,
                    "green" => set.green = parsed_num,
                    "blue" => set.blue = parsed_num,
                    _ => panic!("Unknown color"),
                }
            }
            sets.push(set);
        }
        let game = Game { id, sets };
        games.push(game)
    }
    games
}

/// only 12 red cubes, 13 green cubes, and 14 blue cubes
fn solve_part_1(games: &Vec<Game>) -> u32 {
    let mut sum = 0;
    for game in games {
        let mut valid = true;
        for set in &game.sets {
            // more cubes shown than available, invalid game
            if set.red > 12 || set.green > 13 || set.blue > 14 {
                valid = false;
                break;
            }
        }
        if valid {
            sum += game.id
        }
    }
    sum
}

fn solve_part_2(games: &Vec<Game>) -> u32 {
    let mut sum = 0;
    for game in games {
        let mut min_red = 0;
        let mut min_green = 0;
        let mut min_blue = 0;
        for set in &game.sets {
            if set.red > min_red {
                min_red = set.red
            }
            if set.green > min_green {
                min_green = set.green
            }
            if set.blue > min_blue {
                min_blue = set.blue
            }
        }
        sum += min_red * min_green * min_blue;
    }
    sum
}

fn main() -> std::io::Result<()> {
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
    Ok(())
}
