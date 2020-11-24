use itertools::Itertools;

// Part 1: given a list of weighted packages that need to be split into groups with equal
// weights, what is the lowest product for the first group?  The first group should have
// the smallest number of packages, and the sum of products of all groups should be minimized.
fn part1(data: &Vec<i32>) -> i64 {
    // 1 2 3 4 5 7 8 9 10 11
    // 0 1 2 3 4 5 6 7  8  9

    // 3 groups must have equal weights, so group weight = Σ weight / 3
    let group_weight: i32 = data.iter().sum::<i32>() / 3;

    for n in 1..data.len() {
        let valid = data.iter().combinations(n)
            .filter(|c| c.iter().fold(0, |sum, value| sum + **value) == group_weight)
            .collect_vec();

        if !valid.is_empty() {
            return valid.iter()
                .map(|c| c.iter().fold(1 as i64, |product, value| product * (**value as i64)))
                .min()
                .unwrap_or_default();
        }
    }

    0
}

fn main() {
    // let data = vec![1, 2, 3, 4, 5, 7, 8, 9, 10, 11];
    let data = vec![1, 2, 3, 5, 7, 13, 17, 19, 23, 29, 31, 37, 41, 43,
        53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113];

    println!("Part 1: {}", part1(&data));
}
