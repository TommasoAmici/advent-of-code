use std::collections::{HashMap, HashSet};
use std::fs;
use std::time::Instant;

type X = i32;
type Y = i32;
type PartID = i32;

#[derive(Debug)]
struct Part {
    y: Y,
    xs: HashSet<X>,
}

#[derive(Debug, Default)]
struct Grid {
    parts: HashMap<PartID, Vec<Part>>,
    parts_coords: HashMap<Y, HashMap<X, PartID>>,
    symbols: HashMap<Y, HashSet<X>>,
    possible_gears: HashMap<Y, HashSet<X>>,
}
impl Grid {
    fn insert_symbol(&mut self, y: Y, x: X) {
        if let Some(set) = self.symbols.get_mut(&y) {
            set.insert(x);
        } else {
            let mut new_set = HashSet::new();
            new_set.insert(x);
            self.symbols.insert(y, new_set);
        }
    }

    fn insert_possible_gear(&mut self, y: Y, x: X) {
        if let Some(set) = self.possible_gears.get_mut(&y) {
            set.insert(x);
        } else {
            let mut new_set = HashSet::new();
            new_set.insert(x);
            self.possible_gears.insert(y, new_set);
        }
    }

    fn insert_part(&mut self, part_id: PartID, y: Y, xs: &HashSet<X>) {
        let part = Part { y, xs: xs.clone() };
        if let Some(v) = self.parts.get_mut(&part_id) {
            v.push(part);
        } else {
            self.parts.insert(part_id, vec![part]);
        }
        for x in xs {
            if let Some(map) = self.parts_coords.get_mut(&y) {
                map.insert(*x, part_id);
            } else {
                let mut new_map = HashMap::new();
                new_map.insert(*x, part_id);
                self.parts_coords.insert(y, new_map);
            }
        }
    }
}

fn parse(input: &str) -> Grid {
    let mut grid = Grid::default();
    for (y_index, line) in input.lines().enumerate() {
        let mut partial_num: Vec<char> = Vec::new();
        let mut partial_num_xs: HashSet<X> = HashSet::new();
        for (x_index, c) in line.chars().enumerate() {
            if c.is_numeric() {
                partial_num_xs.insert(x_index.try_into().unwrap());
                partial_num.push(c);
                continue;
            }
            if c == '*' {
                grid.insert_possible_gear(y_index.try_into().unwrap(), x_index.try_into().unwrap());
            }
            if c != '.' {
                grid.insert_symbol(y_index.try_into().unwrap(), x_index.try_into().unwrap());
            }
            if !partial_num.is_empty() {
                let part_str: String = partial_num.iter().collect();
                let part: PartID = part_str.parse().unwrap();
                grid.insert_part(part, y_index.try_into().unwrap(), &partial_num_xs);
                partial_num.clear();
                partial_num_xs.clear();
            }
        }
        if !partial_num.is_empty() {
            let part_str: String = partial_num.iter().collect();
            let part: PartID = part_str.parse().unwrap();
            grid.insert_part(part, y_index.try_into().unwrap(), &partial_num_xs);
            partial_num.clear();
            partial_num_xs.clear();
        }
    }
    grid
}

fn solve_part_1(puzzle: &Grid) -> i32 {
    let mut sum = 0;
    for (id, parts) in &puzzle.parts {
        for part in parts {
            'parts_loop: loop {
                for y in part.y - 1..part.y + 2 {
                    for part_x in &part.xs {
                        for x in part_x - 1..part_x + 2 {
                            if let Some(xs) = &puzzle.symbols.get(&y) {
                                if xs.contains(&x) {
                                    sum += id;
                                    break 'parts_loop;
                                }
                            }
                        }
                    }
                }
                break 'parts_loop;
            }
        }
    }
    sum
}

fn solve_part_2(puzzle: &Grid) -> i32 {
    let mut sum = 0;
    for (gear_y, xs) in &puzzle.possible_gears {
        for gear_x in xs {
            let mut adjacent_parts = HashSet::new();
            for y in gear_y - 1..gear_y + 2 {
                for x in gear_x - 1..gear_x + 2 {
                    if let Some(xs) = &puzzle.parts_coords.get(&y) {
                        if let Some(part_id) = xs.get(&x) {
                            adjacent_parts.insert(*part_id);
                        }
                    }
                }
            }
            if adjacent_parts.len() == 2 {
                sum += adjacent_parts.iter().product::<i32>();
            }
            adjacent_parts.clear();
        }
    }
    sum
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
