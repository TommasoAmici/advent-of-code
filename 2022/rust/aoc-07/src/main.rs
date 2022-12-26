use std::collections::{HashMap, HashSet};
use std::fs;
use std::time::Instant;

#[derive(PartialEq, Eq, Hash, Debug)]
struct File {
    name: String,
    size: i32,
}

#[derive(Debug)]
struct Dir {
    dirs: Box<HashMap<String, Dir>>,
    files: HashSet<File>,
}

fn dir_size(dir: &Dir) -> i32 {
    let mut total = 0;
    for f in dir.files.iter() {
        total += f.size;
    }
    for (_, d) in dir.dirs.iter() {
        total += dir_size(d);
    }
    return total;
}

fn build_fs(input: &str) -> Dir {
    let mut path = vec![];
    let mut file_system = Dir {
        dirs: Default::default(),
        files: Default::default(),
    };
    for line in input.lines() {
        if line.is_empty() || line.starts_with("$ ls") {
            continue;
        }

        if line.starts_with("$ cd") {
            let (_, next_dir) = line.split_once("cd ").expect("cd: failed to read dirname");
            if next_dir == "/" {
                path = vec![];
            } else if next_dir == ".." {
                path.pop();
            } else {
                path.push(next_dir);
            }
        } else if line.starts_with("dir") {
            let (_, name) = line.split_once(" ").expect("ls: failed to read dirname");
            let mut curr: &mut Dir = &mut file_system;
            for p in path.iter() {
                if curr.dirs.contains_key(*p) {
                    curr = curr.dirs.get_mut(*p).expect("failed to get directory");
                    continue;
                } else {
                    curr.dirs.insert(
                        p.to_string(),
                        Dir {
                            dirs: Default::default(),
                            files: Default::default(),
                        },
                    );
                    curr = curr.dirs.get_mut(*p).expect("failed to get directory");
                }
            }
            curr.dirs.insert(
                name.to_string(),
                Dir {
                    dirs: Default::default(),
                    files: Default::default(),
                },
            );
        } else {
            let (_size, name) = line
                .split_once(" ")
                .expect("ls: Failed to read file and size");
            let size: i32 = _size.parse().expect("ls: failed to parse size");
            let mut curr: &mut Dir = &mut file_system;
            for p in path.iter() {
                curr = curr.dirs.get_mut(*p).expect("failed to get directory");
            }
            curr.files.insert(File {
                name: name.to_string(),
                size: size,
            });
        }
    }
    return file_system;
}

fn solve_part_1(curr: &Dir) -> i32 {
    let mut total = 0;

    for (_, dir) in curr.dirs.iter() {
        let size = dir_size(dir);
        if size < 100_000 {
            total += size;
        }
        total += solve_part_1(dir);
    }

    return total;
}

const TOTAL_SPACE: i32 = 70_000_000;
const SPACE_FOR_UPGRADE: i32 = 30_000_000;

fn solve_part_2(curr: &Dir, space_needed: i32) -> i32 {
    let mut smallest = TOTAL_SPACE;
    for (_, dir) in curr.dirs.iter() {
        let size = dir_size(dir);
        if size >= space_needed && size <= smallest {
            smallest = size;
        }
        let _nested_smallest = solve_part_2(dir, space_needed);
        if _nested_smallest <= smallest {
            smallest = _nested_smallest;
        }
    }
    return smallest;
}

fn main() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");

    let parse_start = Instant::now();
    let file_system = build_fs(&contents);
    println!("parsing: {}µs", parse_start.elapsed().as_micros());

    let part_1_start = Instant::now();
    let part_1 = solve_part_1(&file_system);
    println!(
        "part 1: {} {}µs",
        part_1,
        part_1_start.elapsed().as_micros()
    );
    let part_2_start = Instant::now();
    let space_used = dir_size(&file_system);
    let space_needed = SPACE_FOR_UPGRADE - (TOTAL_SPACE - space_used);
    let part_2 = solve_part_2(&file_system, space_needed);
    println!(
        "part 2: {} {}µs",
        part_2,
        part_2_start.elapsed().as_micros()
    );
}
