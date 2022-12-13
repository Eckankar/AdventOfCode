use std::{
    cmp::Ordering,
    io::stdin
};

use nom::{
    IResult,
    branch::alt,
    character::complete::{char,digit1},
    combinator::{map, map_res},
    multi::separated_list0,
    sequence::delimited
};

#[derive(Clone, Debug, PartialEq, Eq)]
enum NList {
    NNum(u8),
    NList(Vec<NList>)
}

impl PartialOrd for NList {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn vecify(l: Vec<NList>) -> Vec<NList> {
    l
}

impl Ord for NList {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (NList::NNum(v1),  NList::NNum(v2))  => v1.cmp(v2),
            (NList::NList(v1), NList::NList(v2)) => v1.cmp(v2),
            (v1,               NList::NList(v2)) => vecify(vec![v1.clone()]).cmp(&v2),
            (NList::NList(v1), v2)               => v1.cmp(&vecify(vec![v2.clone()]))
        }
    }
}

fn parse_nlist(input: &str) -> IResult<&str, NList> {
    alt((
        map_res(digit1, |s| u8::from_str_radix(s, 10).map(NList::NNum)),
        map(
            delimited(
                char('['),
                separated_list0(char(','), parse_nlist),
                char(']')
            ),
            NList::NList
        )
    ))(input)
}

fn parse_line(line: String) -> Option<NList> {
    if line.is_empty() {
        return None;
    }

    parse_nlist(&line).map(|(_,v)| v).ok()
}

fn solve_part1(inputs: Vec<NList>) -> usize {
    inputs.chunks(2)
          .enumerate()
          .filter(|(_, l)| match l { [n1, n2] => n1 <= n2, _ => false })
          .map(|(n,_)| n+1)
          .sum()
}

fn is_sentry(v: &NList) -> bool {
    match v {
        NList::NList(v2) => v2.len() == 1 && match &v2[0] {
            NList::NList(v3) => v3.len() == 1 && match &v3[0] {
                NList::NNum(2) => true,
                NList::NNum(6) => true,
                _  => false
                },
            _ => false
            },
        _ => false
    }
}

fn solve_part2(inputs: Vec<NList>) -> usize {
    let mut inputs_mut = inputs.clone();

    inputs_mut.extend([
        NList::NList( vec![ NList::NList( vec![ NList::NNum(2) ] ) ]),
        NList::NList( vec![ NList::NList( vec![ NList::NNum(6) ] ) ]),
    ]);

    inputs_mut.sort();

    inputs_mut.iter()
              .enumerate()
              .filter(|(_, l)| is_sentry(l))
              .map(|(n,_)| n+1)
              .fold(1, |acc, x| acc * x)
}

fn main() {
    let inputs : Vec<NList> = stdin()
        .lines()
        .filter_map(|l| parse_line(l.unwrap()))
        .collect();

    println!("Part 1: {}", solve_part1(inputs.clone()));
    println!("Part 2: {}", solve_part2(inputs.clone()));
}
