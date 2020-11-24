fn code(row: usize, col: usize) -> u64 {
    let mut r = 1;
    let mut c = 1;
    let mut code: u64 = 20151125;

    loop {
        if r == row && c == col {
            break;
        }

        code = code * 252533 % 33554393;

        r = r - 1;
        c = c + 1;

        if r == 0 {
            r = c;
            c = 1;
        }
    }

    code
}

fn main() {
    println!("Part 1: {}", code(3010, 3019))
}
