#[derive(Clone, Debug)]
struct Loadout {
    group1: Vec<i32>,
    group2: Vec<i32>,
    group3: Vec<i32>,
}

// Part 1: given a list of weighted packages that need to be split into groups with equal
// weights, what is the lowest product for the first group?  The first group should have
// the smallest number of packages, and the sum of products of all groups should be minimized.
fn part1(data: &[i32]) -> i32 {
    // 1 2 3 4 5 7 8 9 10 11
    // 0 1 2 3 4 5 6 7  8  9

    // 3 groups must have equal weights, so group weight = Σ weight / 3
    let group_weight: i32 = data.iter().sum::<i32>() / 3;

    let init_loadout = Loadout {
        group1: Vec::new(),
        group2: Vec::new(),
        group3: Vec::new(),
    };

    let loadouts = find_loadouts(data, group_weight, data.len(), init_loadout);

    println!("{:?}", loadouts.clone());

    // Tie-breakers:
    // smallest number of packages in the first group.
    // smallest total quantum entanglement (sum of the product of weights in each group)

    let min_packages = loadouts.iter()
        .map(|loadout| loadout.group1.len())
        .min()
        .unwrap_or_default();

    loadouts.into_iter()
        .filter(|loadout| loadout.group1.len() == min_packages)
        .min_by_key(entanglement)
        .map(|loadout| loadout.group1.iter().fold(1, |product, &value| product * value))
        .unwrap_or_default()
}

fn entanglement(loadout: &Loadout) -> i32 {
    let product_fn = |product, &value| product * value;

    loadout.group1.iter().fold(1, product_fn)
        + loadout.group2.iter().fold(1, product_fn)
        + loadout.group3.iter().fold(1, product_fn)
}

fn find_loadouts(data: &[i32], target: i32, i: usize, so_far: Loadout) -> Vec<Loadout> {
    println!("{}", i);

    if i == 0 {
        return vec!(so_far.clone())
    }

    let mut loadouts = Vec::new();
    if so_far.group1.iter().sum::<i32>() + data[i - 1] <= target {
        let mut new_group1= so_far.group1.clone();
        new_group1.push(data[i - 1]);

        let load = Loadout {
            group1: new_group1.to_owned(),
            group2: so_far.group2.clone(),
            group3: so_far.group3.clone(),
        };

        loadouts.extend(find_loadouts(data, target, i-1, load))
    }

    if so_far.group2.iter().sum::<i32>() + data[i - 1] <= target {
        let mut new_group2= so_far.group2.clone();
        new_group2.push(data[i - 1]);

        let load = Loadout {
            group1: so_far.group1.clone(),
            group2: new_group2.to_owned(),
            group3: so_far.group3.clone(),
        };

        loadouts.extend(find_loadouts(data, target, i-1, load))
    }

    if so_far.group3.iter().sum::<i32>() + data[i - 1] <= target {
        let mut new_group3= so_far.group3.clone();
        new_group3.push(data[i - 1]);

        let load = Loadout {
            group1: so_far.group1.clone(),
            group2: so_far.group2.clone(),
            group3: new_group3.to_owned(),
        };

        loadouts.extend(find_loadouts(data, target, i-1, load))
    }

    loadouts
}

fn main() {
    // let data = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11];
    let data = [1, 2, 3, 5, 7, 13, 17, 19, 23, 29, 31, 37, 41, 43,
        53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113];

    println!("Part 1: {}", part1(&data));
}
