use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::complete;
use nom::multi::count;
use nom::number::complete::{be_f64, be_i32, be_u16, be_u32, be_u8};
use nom::sequence::pair;
use nom::IResult;
use std::collections::BTreeMap;

type ParseResult<'a, T> = std::result::Result<(T, Term<'a>), nom::Err<(T, nom::error::ErrorKind)>>;

// alphabetical order
const ATOM_TAG: &[u8] = &[100];
const BINARY_TAG: &[u8] = &[109];
const INTEGER_TAG: &[u8] = &[98];
const LARGE_BIG_TAG: &[u8] = &[111];
const LIST_TAG: &[u8] = &[108];
const MAP_TAG: &[u8] = &[116];
const NEW_FLOAT_TAG: &[u8] = &[70];
const NIL_TAG: &[u8] = &[106];
const SMALL_BIG_TAG: &[u8] = &[110];
const SMALL_INTEGER_TAG: &[u8] = &[97];
const VERSION_NUMBER_TAG: &[u8] = &[131];

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Term<'a> {
    Atom(&'a str),
    Binary(&'a str),
    Integer(i32),
    List(Vec<Term<'a>>),
    Map(BTreeMap<Term<'a>, Term<'a>>),
    NewFloat(f64),
    Nil,
    SmallInteger(u8),
    SmallBig(num_bigint::BigInt),
    LargeBig(num_bigint::BigInt),
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
        binary,
        atom,
        map,
        list,
        integer,
        new_float,
        nil,
        small_integer,
        small_big,
        large_big,
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
    let (s, sign_byte) = be_u8(s)?;
    let (s, digits) = count(be_u8, n as usize)(s)?;

    let sign = if sign_byte == 1 {
        num_bigint::Sign::Minus
    } else {
        num_bigint::Sign::Plus
    };

    Ok((
        s,
        Term::SmallBig(num_bigint::BigInt::from_radix_le(sign, &digits, 256).unwrap()),
    ))
}

fn large_big(s: &[u8]) -> IResult<&[u8], Term> {
    let (s, _) = tag(LARGE_BIG_TAG)(s)?;
    let (s, n) = be_u32(s)?;
    let (s, sign_byte) = be_u8(s)?;
    let (s, digits) = count(be_u8, n as usize)(s)?;

    let sign = if sign_byte == 1 {
        num_bigint::Sign::Minus
    } else {
        num_bigint::Sign::Plus
    };

    Ok((
        s,
        Term::LargeBig(num_bigint::BigInt::from_radix_le(sign, &digits, 256).unwrap()),
    ))
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
    let (s, _term) = nil(s)?;

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
        let (remaining, parsed) = parse(&string).unwrap();
        assert!(remaining.is_empty());
        assert_eq!(parsed, Term::Binary("hello"));
    }

    #[test]
    fn empty_map() {
        let empty_map = [131, 116, 0, 0, 0, 0];
        let (remaining, parsed) = parse(&empty_map).unwrap();
        assert!(remaining.is_empty());
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
        let (remaining, parsed) = parse(&map).unwrap();
        assert!(remaining.is_empty());
        assert_eq!(parsed, Term::Map(btm));
    }

    #[test]
    fn empty_list() {
        let empty_list = [131, 106];
        let (remaining, parsed) = parse(&empty_list).unwrap();
        assert!(remaining.is_empty());
        assert_eq!(parsed, Term::Nil);
    }

    #[test]
    fn list() {
        let list = [
            131, 108, 0, 0, 0, 1, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 106,
        ];
        let (remaining, parsed) = parse(&list).unwrap();
        let expected = Term::List(vec![Term::Binary("hello")]);
        assert!(remaining.is_empty());
        assert_eq!(parsed, expected);
    }

    #[test]
    fn small_integer() {
        let small_int = [131, 97, 12];
        let (remaining, parsed) = parse(&small_int).unwrap();
        let expected = Term::SmallInteger(12);
        assert!(remaining.is_empty());
        assert_eq!(parsed, expected);
    }

    #[test]
    fn integer() {
        let int = [131, 98, 255, 255, 255, 244];
        let (remaining, parsed) = parse(&int).unwrap();
        let expected = Term::Integer(-12);
        assert!(remaining.is_empty());
        assert_eq!(parsed, expected);
    }

    #[test]
    fn new_float() {
        let float = [131, 70, 64, 64, 12, 204, 204, 204, 204, 205];
        let (remaining, parsed) = parse(&float).unwrap();
        let expected = Term::NewFloat(32.1);
        assert!(remaining.is_empty());
        assert_eq!(parsed, expected);
    }

    #[test]
    fn small_big() {
        // a big number printed from an iex console
        let small_big = [
            131, 110, 14, 0, 199, 113, 28, 199, 171, 237, 49, 63, 73, 243, 92, 107, 122, 5,
        ];

        let (remaining, parsed) = parse(&small_big).unwrap();

        let expected = Term::SmallBig(
            num_bigint::BigInt::from_radix_le(
                num_bigint::Sign::Plus,
                &[
                    199, 113, 28, 199, 171, 237, 49, 63, 73, 243, 92, 107, 122, 5,
                ],
                256,
            )
            .unwrap(),
        );
        assert!(remaining.is_empty());
        assert_eq!(parsed, expected);
    }
    #[test]
    fn large_big() {
        // a really big number printed from an iex console
        let large_big = [
            131, 111, 0, 0, 1, 17, 0, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28,
            199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28,
            199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28,
            199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28,
            199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 171, 73, 10, 20,
            34, 212, 21, 226, 95, 123, 248, 214, 6, 53, 243, 229, 35, 19, 174, 76, 156, 39, 218,
            47, 2, 218, 1, 131, 80, 141, 17, 13, 230, 131, 61, 130, 201, 110, 22, 160, 204, 187,
            230, 8, 227, 198, 134, 77, 167, 137, 116, 53, 70, 144, 160, 12, 160, 102, 159, 78, 254,
            49, 155, 44, 109, 161, 58, 241, 149, 157, 156, 217, 13, 207, 73, 19, 217, 223, 157, 49,
            233, 36, 110, 227, 185, 63, 139, 63, 194, 106, 174, 247, 236, 207, 91, 155, 181, 121,
            152, 222, 251, 92, 87, 56, 160, 64, 208, 164, 171, 64, 32, 197, 205, 168, 66, 248, 248,
            78, 186, 129, 126, 96, 0, 202, 176, 40, 119, 112, 206, 95, 195, 217, 122, 135, 246, 99,
            150, 134, 55, 75, 233, 113, 130, 86, 248, 96, 29, 20, 168, 118, 55, 194, 254, 217, 225,
            204, 14, 45, 56, 52, 33, 218, 221, 106, 209, 33, 122, 132, 162, 236, 106, 134, 75, 63,
            144, 142, 238, 212, 224, 43, 122, 189, 232, 10, 100, 123, 28, 99, 162, 10, 4,
        ];

        let (remaining, parsed) = parse(&large_big).unwrap();

        let expected = Term::LargeBig(
            num_bigint::BigInt::from_radix_le(
                num_bigint::Sign::Plus,
                &[
                    28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199,
                    113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28,
                    199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113,
                    28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199,
                    113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 199, 113, 28, 171, 73, 10,
                    20, 34, 212, 21, 226, 95, 123, 248, 214, 6, 53, 243, 229, 35, 19, 174, 76, 156,
                    39, 218, 47, 2, 218, 1, 131, 80, 141, 17, 13, 230, 131, 61, 130, 201, 110, 22,
                    160, 204, 187, 230, 8, 227, 198, 134, 77, 167, 137, 116, 53, 70, 144, 160, 12,
                    160, 102, 159, 78, 254, 49, 155, 44, 109, 161, 58, 241, 149, 157, 156, 217, 13,
                    207, 73, 19, 217, 223, 157, 49, 233, 36, 110, 227, 185, 63, 139, 63, 194, 106,
                    174, 247, 236, 207, 91, 155, 181, 121, 152, 222, 251, 92, 87, 56, 160, 64, 208,
                    164, 171, 64, 32, 197, 205, 168, 66, 248, 248, 78, 186, 129, 126, 96, 0, 202,
                    176, 40, 119, 112, 206, 95, 195, 217, 122, 135, 246, 99, 150, 134, 55, 75, 233,
                    113, 130, 86, 248, 96, 29, 20, 168, 118, 55, 194, 254, 217, 225, 204, 14, 45,
                    56, 52, 33, 218, 221, 106, 209, 33, 122, 132, 162, 236, 106, 134, 75, 63, 144,
                    142, 238, 212, 224, 43, 122, 189, 232, 10, 100, 123, 28, 99, 162, 10, 4,
                ],
                256,
            )
            .unwrap(),
        );
        assert!(remaining.is_empty());
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

        let (remaining, parsed) = parse(&small_map).unwrap();
        let mut map = BTreeMap::new();
        map.insert(Term::Atom("a"), Term::SmallInteger(73));
        map.insert(Term::Atom("b"), Term::Integer(8248));
        map.insert(Term::Atom("c"), Term::Binary("hello"));
        map.insert(Term::Atom("d"), Term::Atom("ok"));
        map.insert(Term::Atom("e"), Term::SmallInteger(99));
        let expected = Term::Map(map);
        assert!(remaining.is_empty());
        assert_eq!(parsed, expected);
    }
}
