use rand::prelude::*;

pub fn private_key(p: u64) -> u64 {
    rand::thread_rng().gen_range(2..p)
}

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    exp_mod(p, g, a)
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    exp_mod(p, b_pub, a)
}

fn exp_mod(modulus: u64, base: u64, exp: u64) -> u64 {
    let modulus: u128 = modulus as u128;
    let base: u128 = base as u128;
    let exp: u128 = exp as u128;

    std::iter::successors((exp > 0).then_some(exp), |&exp| {
        (exp > 1).then_some(exp >> 1)
    })
    .map(|exp| exp & 1 == 1)
    .fold((1, base), |(result, base), bit| {
        (
            if bit {
                (result * base) % modulus
            } else {
                result
            },
            (base * base) % modulus,
        )
    })
    .0 as u64
}
