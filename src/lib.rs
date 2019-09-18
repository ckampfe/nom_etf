use nom::branch::alt;
use nom::bytes::streaming::tag;
use nom::bytes::streaming::take;
use nom::combinator::complete;
use nom::multi::count;
use nom::number::streaming::{be_f64, be_i32, be_u16, be_u32, be_u8};
use nom::sequence::pair;
use nom::IResult;
use std::collections::BTreeMap;
use serde::Serialize;

type ParseResult<'a, T> = std::result::Result<(T, Term<'a>), nom::Err<(T, nom::error::ErrorKind)>>;

const VERSION_NUMBER_TAG: &[u8] = &[131];
const NEW_FLOAT_TAG: &[u8] = &[70];
const SMALL_INTEGER_TAG: &[u8] = &[97];
const INTEGER_TAG: &[u8] = &[98];
const ATOM_TAG: &[u8] = &[100];
const NIL_TAG: &[u8] = &[106];
const LIST_TAG: &[u8] = &[108];
const BINARY_TAG: &[u8] = &[109];
const SMALL_BIG_TAG: &[u8] = &[110];
const MAP_TAG: &[u8] = &[116];

const B: i128 = 256;

#[derive(Clone, Debug, PartialEq, PartialOrd, Serialize)]
pub enum Term<'a> {
    Atom(&'a str),
    Binary(&'a str),
    Integer(i32),
    List(Vec<Term<'a>>),
    Map(BTreeMap<Term<'a>, Term<'a>>),
    NewFloat(f64),
    Nil,
    SmallInteger(u8),
    SmallBig(i128),
    LargeBig(i128),
}

impl Ord for Term<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match &self {
            Term::NewFloat(f) => match other {
                Term::NewFloat(other_f) => {
                    f.partial_cmp(other_f).unwrap_or(std::cmp::Ordering::Equal)
                }
                _ => panic!("can't compare"),
            },
            _ => self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal),
        }
    }
}
impl Eq for Term<'_> {}

pub fn parse(bytes: &[u8]) -> ParseResult<&[u8]> {
    let (s, _) = tag(VERSION_NUMBER_TAG)(bytes)?;
    let (s, term) = complete(term)(s)?;
    Ok((s, term))
}

fn term(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, res) = alt((
        atom,
        binary,
        small_integer,
        small_big,
        integer,
        list,
        map,
        new_float,
        nil,
    ))(s)?;
    Ok((s, res))
}

fn binary(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(BINARY_TAG)(s)?;
    let (s, len) = be_u32(s)?;
    let (s, string) = take(len)(s)?;

    Ok((s, Term::Binary(std::str::from_utf8(string).unwrap())))
}

fn new_float(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(NEW_FLOAT_TAG)(s)?;
    let (s, float) = be_f64(s)?;
    Ok((s, Term::NewFloat(float)))
}

fn small_integer(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(SMALL_INTEGER_TAG)(s)?;
    let (s, small_integer) = be_u8(s)?;
    Ok((s, Term::SmallInteger(small_integer)))
}

fn small_big(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(SMALL_BIG_TAG)(s)?;
    let (s, n) = be_u8(s)?;
    let (s, sign) = be_u8(s)?;
    let (s, digits) = count(be_u8, n as usize)(s)?;

    let mut small_big = 0i128;
    // (d0*B^0 + d1*B^1 + d2*B^2 + ... d(N-1)*B^(n-1))
    for (i, digit) in digits.into_iter().enumerate() {
        small_big += i128::from(digit) * i128::pow(B, i as u32)
    }

    if sign == 1 {
        small_big *= -1
    }

    Ok((s, Term::SmallBig(small_big)))
}

fn integer(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(INTEGER_TAG)(s)?;
    let (s, integer) = be_i32(s)?;
    Ok((s, Term::Integer(integer)))
}

fn map(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(MAP_TAG)(s)?;
    let (s, number_of_pairs) = be_u32(s)?;
    let (s, pairs) = count(pair(term, term), number_of_pairs as usize)(s)?;
    let mut hm = BTreeMap::new();
    for (k, v) in pairs {
        hm.insert(k, v);
    }

    Ok((s, Term::Map(hm)))
}

fn nil(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(NIL_TAG)(s)?;
    Ok((s, Term::Nil))
}

fn list(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(LIST_TAG)(s)?;
    let (s, number_of_elements) = be_u32(s)?;
    let (s, elements) = count(term, number_of_elements as usize)(s)?;
    Ok((s, Term::List(elements)))
}

fn atom(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(ATOM_TAG)(s)?;
    let (s, len) = be_u16(s)?;
    let (s, string) = take(len)(s)?;

    Ok((s, Term::Atom(std::str::from_utf8(string).unwrap())))
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn string() {
        let string = [131, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111];
        let (_remaining, parsed) = parse(&string).unwrap();
        assert_eq!(parsed, Term::Binary("hello"));
    }

    #[test]
    fn empty_map() {
        let empty_map = [131, 116, 0, 0, 0, 0];
        let (_remaining, parsed) = parse(&empty_map).unwrap();
        assert_eq!(parsed, Term::Map(BTreeMap::new()));
    }

    #[test]
    fn string_map() {
        let map = [
            131, 116, 0, 0, 0, 1, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 109, 0, 0, 0, 5, 116,
            104, 101, 114, 101,
        ];

        let mut btm = BTreeMap::new();
        btm.insert(Term::Binary("hello"), Term::Binary("there"));
        let (_remaining, parsed) = parse(&map).unwrap();
        assert_eq!(parsed, Term::Map(btm));
    }

    #[test]
    fn empty_list() {
        let empty_list = [131, 106];
        let (_remaining, parsed) = parse(&empty_list).unwrap();
        assert_eq!(parsed, Term::Nil);
    }

    #[test]
    fn list() {
        let list = [
            131, 108, 0, 0, 0, 1, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 106,
        ];
        let (_remaining, parsed) = parse(&list).unwrap();
        let expected = Term::List(vec![Term::Binary("hello")]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn small_integer() {
        let small_int = [131, 97, 12];
        let (_remaining, parsed) = parse(&small_int).unwrap();
        let expected = Term::SmallInteger(12);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn integer() {
        let int = [131, 98, 255, 255, 255, 244];
        let (_remaining, parsed) = parse(&int).unwrap();
        let expected = Term::Integer(-12);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn new_float() {
        let float = [131, 70, 64, 64, 12, 204, 204, 204, 204, 205];
        let (_remaining, parsed) = parse(&float).unwrap();
        let expected = Term::NewFloat(32.1);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn small_big() {
        let small_big = [
            131, 110, 14, 0, 199, 113, 28, 199, 171, 237, 49, 63, 73, 243, 92, 107, 122, 5,
        ];
        let (_remaining, parsed) = parse(&small_big).unwrap();
        let expected = Term::SmallBig(111111111111111111111111111111111);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn heterogeneous_small_map() {
        // {:a 73 :b 8248 :c "hello" :d :ok :e 99}
        let small_map = [
            131, 116, 0, 0, 0, 5, 100, 0, 1, 97, 97, 73, 100, 0, 1, 98, 98, 0, 0, 32, 56, 100, 0,
            1, 99, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 100, 0, 1, 100, 100, 0, 2, 111, 107,
            100, 0, 1, 101, 97, 99,
        ];

        let (_remaining, parsed) = parse(&small_map).unwrap();
        let mut map = BTreeMap::new();
        map.insert(Term::Atom("a"), Term::SmallInteger(73));
        map.insert(Term::Atom("b"), Term::Integer(8248));
        map.insert(Term::Atom("c"), Term::Binary("hello"));
        map.insert(Term::Atom("d"), Term::Atom("ok"));
        map.insert(Term::Atom("e"), Term::SmallInteger(99));
        let expected = Term::Map(map);
        assert_eq!(parsed, expected);
    }
}
