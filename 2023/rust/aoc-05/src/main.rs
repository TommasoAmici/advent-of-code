use std::fs;
use std::time::Instant;

#[derive(Debug)]
struct Range {
    source: u64,
    dest: u64,
    len: u64,
}

type Seed = u64;

#[derive(Debug)]
struct Almanac {
    seeds: Vec<Seed>,
    seeds_range: Vec<(Seed, Seed)>,
    seed_to_soil: Vec<Range>,
    soil_to_fertilizer: Vec<Range>,
    fertilizer_to_water: Vec<Range>,
    water_to_light: Vec<Range>,
    light_to_temperature: Vec<Range>,
    temperature_to_humidity: Vec<Range>,
    humidity_to_location: Vec<Range>,
}

impl Almanac {
    fn convert(&self, map: &Vec<Range>, value: Seed) -> u64 {
        let range = map
            .iter()
            .find(|s| value >= s.source && value < s.source + s.len);
        match range {
            None => value,
            Some(r) => value - r.source + r.dest,
        }
    }

    fn find_seed_location(&self, seed: Seed) -> u64 {
        let soil = self.convert(&self.seed_to_soil, seed);
        let fertilizer = self.convert(&self.soil_to_fertilizer, soil);
        let water = self.convert(&self.fertilizer_to_water, fertilizer);
        let light = self.convert(&self.water_to_light, water);
        let temperature = self.convert(&self.light_to_temperature, light);
        let humidity = self.convert(&self.temperature_to_humidity, temperature);
        self.convert(&self.humidity_to_location, humidity)
    }

    fn all_seeds_locations(&self) -> Vec<u64> {
        self.seeds
            .iter()
            .map(|s| self.find_seed_location(*s))
            .collect()
    }

    fn parse_ranges(range: &str) -> Vec<Range> {
        let mut ranges = Vec::new();
        for line in range.lines().skip(1) {
            let parts: Vec<u64> = line
                .split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect();
            let dest = parts[0];
            let source = parts[1];
            let len = parts[2];
            let r = Range { source, dest, len };
            ranges.push(r);
        }
        ranges
    }

    fn from(input: &str) -> Almanac {
        let parts: Vec<&str> = input.split("\n\n").collect();
        let seeds: Vec<Seed> = parts[0]
            .replace("seeds: ", "")
            .split_whitespace()
            .map(|x| x.parse().unwrap())
            .collect();
        let mut seeds_range: Vec<(Seed, Seed)> = Vec::new();
        let mut seed_start = None;
        for seed in seeds.clone() {
            match seed_start {
                None => {
                    seed_start = Some(seed);
                }
                Some(s) => {
                    seeds_range.push((s, seed));
                    seed_start = None;
                }
            }
        }
        let seed_to_soil = Almanac::parse_ranges(parts[1]);
        let soil_to_fertilizer = Almanac::parse_ranges(parts[2]);
        let fertilizer_to_water = Almanac::parse_ranges(parts[3]);
        let water_to_light = Almanac::parse_ranges(parts[4]);
        let light_to_temperature = Almanac::parse_ranges(parts[5]);
        let temperature_to_humidity = Almanac::parse_ranges(parts[6]);
        let humidity_to_location = Almanac::parse_ranges(parts[7]);
        Almanac {
            seeds,
            seeds_range,
            seed_to_soil,
            soil_to_fertilizer,
            fertilizer_to_water,
            water_to_light,
            light_to_temperature,
            temperature_to_humidity,
            humidity_to_location,
        }
    }
}

fn solve_part_1(almanac: &Almanac) -> u64 {
    let locations = almanac.all_seeds_locations();
    *locations.iter().min().unwrap()
}

fn solve_part_2_brute_force(almanac: &Almanac) -> u64 {
    let mut min = 0;
    for (start, end) in &almanac.seeds_range {
        for seed in *start..start + end {
            let location = almanac.find_seed_location(seed);
            if min == 0 {
                min = location;
            } else if location < min {
                min = location
            }
        }
    }
    min
}

fn solve_part_2(almanac: &Almanac) -> u64 {
    todo!()
}

fn main() {
    let file_path = "./data/input.txt";
    let contents = fs::read_to_string(file_path).expect("Failed to open file");

    let parse_start = Instant::now();
    let puzzle = Almanac::from(&contents);
    println!("parsing: {}µs", parse_start.elapsed().as_micros());

    let part_1_start = Instant::now();
    let part_1 = solve_part_1(&puzzle);
    println!("{} {}µs", part_1, part_1_start.elapsed().as_micros());

    let part_2_start = Instant::now();
    let part_2 = solve_part_2_brute_force(&puzzle);
    println!("{} {}µs", part_2, part_2_start.elapsed().as_micros());

    let part_2_start = Instant::now();
    let part_2 = solve_part_2(&puzzle);
    println!("{} {}µs", part_2, part_2_start.elapsed().as_micros());
}

#[test]
fn test_solve_part_1() {
    let case = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";
    let puzzle = Almanac::from(case);
    assert_eq!(solve_part_1(&puzzle), 35);
}
