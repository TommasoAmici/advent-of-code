use std::fs;
use std::time::Instant;

fn solve_part_1(puzzle: &str) -> i32 {
    let mut cycle = 0;
    let mut total_signal_strength = 0;
    let mut signal_strength = 1;

    for line in puzzle.lines() {
        cycle += 1;

        if cycle % 40 == 20 {
            total_signal_strength += signal_strength * cycle;
        }

        match line {
            "noop" => continue,
            l => {
                let (_, value) = l.split_once(" ").expect("addx instruction");
                let add_x: i32 = value.parse().expect("expected a number");

                cycle += 1;

                if cycle % 40 == 20 {
                    total_signal_strength += signal_strength * cycle;
                }

                signal_strength += add_x;
            }
        }
    }
    return total_signal_strength;
}

fn draw_crt(cycle: i32, signal_strength: i32) {
    let x = cycle % 40;
    if x >= signal_strength - 1 && x <= signal_strength + 1 {
        print!("█");
    } else {
        print!(" ");
    }

    if (cycle + 1) % 40 == 0 {
        println!();
    }
}

fn solve_part_2(puzzle: &str) {
    let mut cycle = 0;
    let mut signal_strength = 1;

    for line in puzzle.lines() {
        match line {
            "noop" => {
                draw_crt(cycle, signal_strength);
                cycle += 1;
            }
            l => {
                draw_crt(cycle, signal_strength);
                cycle += 1;

                let (_, value) = l.split_once(" ").expect("addx instruction");
                let add_x: i32 = value.parse().expect("expected a number");

                draw_crt(cycle, signal_strength);
                cycle += 1;
                signal_strength += add_x;
            }
        }
    }
}

fn main() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");

    let part_1_start = Instant::now();
    let part_1 = solve_part_1(&contents);
    println!("{} {}µs", part_1, part_1_start.elapsed().as_micros());

    let part_2_start = Instant::now();
    solve_part_2(&contents);
    println!("{}µs", part_2_start.elapsed().as_micros());
}
