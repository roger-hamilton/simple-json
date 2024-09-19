use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{anychar, char, multispace0, none_of},
    combinator::{map, map_opt, map_res, value, verify},
    error::ParseError,
    multi::{fold_many0, separated_list0},
    sequence::{delimited, preceded, separated_pair},
    AsChar, IResult, InputTakeAtPosition, Parser,
};

type Res<'a, T> = IResult<&'a str, T>;

fn parse_bool(s: &str) -> Res<bool> {
    let parse_true = map(tag("true"), |_| true);
    let parse_false = map(tag("false"), |_| false);

    alt((parse_true, parse_false))(s)
}

fn jbool(s: &str) -> Res<JsonValue> {
    map(parse_bool, JsonValue::Bool)(s)
}

fn jnum(s: &str) -> Res<JsonValue> {
    map(nom::number::complete::double, JsonValue::Num)(s)
}

fn u16_hex<'a>() -> impl FnMut(&'a str) -> Res<'a, u16> {
    map_res(take(4usize), |s| u16::from_str_radix(s, 16))
}

fn unicode_escaped<'a>() -> impl FnMut(&'a str) -> Res<'a, char> {
    let single_escaped = verify(u16_hex(), |cp| !(0xD800..0xE000).contains(cp));

    let double_escaped = verify(
        separated_pair(u16_hex(), tag("\\u"), u16_hex()),
        |(first, second)| (0xD800..0xDC00).contains(first) && (0xDC00..0xE000).contains(second),
    );
    map_opt(
        alt((
            map(single_escaped, |cp| cp as u32),
            map(double_escaped, |(high, low)| {
                let high_ten = (high - 0xD800) as u32;
                let low_ten = (low - 0xDC00) as u32;
                0x10000 + (high_ten << 10) + low_ten
            }),
        )),
        std::char::from_u32,
    )
}

fn string<'a>() -> impl FnMut(&'a str) -> Res<'a, String> {
    let character = move |input| {
        let (input, c) = none_of("\"")(input)?;
        if c == '\\' {
            alt((
                map_res(anychar, |c| {
                    Ok(match c {
                        '"' | '\\' | '/' => c,
                        'b' => '\x08',
                        'f' => '\x0C',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        _ => return Err(()),
                    })
                }),
                preceded(char('u'), unicode_escaped()),
            ))(input)
        } else {
            Ok((input, c))
        }
    };

    delimited(
        char('"'),
        fold_many0(character, String::new, |mut string, c| {
            string.push(c);
            string
        }),
        char('"'),
    )
}

fn jnull(s: &str) -> Res<JsonValue> {
    value(JsonValue::Null, tag("null"))(s)
}

fn jstr(s: &str) -> Res<JsonValue> {
    map(string(), JsonValue::Str)(s)
}

fn ws<I, O, E: ParseError<I>>(p: impl Parser<I, O, E>) -> impl Parser<I, O, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: Clone + AsChar,
{
    delimited(multispace0, p, multispace0)
}

fn jarr(s: &str) -> Res<JsonValue> {
    map(
        delimited(
            char('['),
            ws(separated_list0(ws(char(',')), jvalue)),
            char(']'),
        ),
        JsonValue::Array,
    )(s)
}

fn jobj(s: &str) -> Res<JsonValue> {
    map(
        delimited(
            char('{'),
            ws(separated_list0(
                ws(char(',')),
                separated_pair(string(), ws(char(':')), jvalue),
            )),
            char('}'),
        ),
        JsonValue::Object,
    )(s)
}

fn jvalue(s: &str) -> Res<JsonValue> {
    alt((jnull, jbool, jnum, jstr, jarr, jobj))(s)
}

#[derive(Debug, PartialEq, Clone)]
enum JsonValue {
    Null,
    Bool(bool),
    Str(String),
    Num(f64),
    Array(Vec<JsonValue>),
    Object(Vec<(String, JsonValue)>),
}

fn main() {
    let inp = "[1, 2, 3]";
    let res = jvalue(inp);

    println!("{:?}", res);
}

#[cfg(test)]
mod test {
    use rstest::rstest;

    use super::*;
    use JsonValue::*;

    #[rstest]
    fn null() {
        assert_eq!(jnull("null"), Ok(("", Null)));
    }

    #[rstest]
    #[case("true", true)]
    #[case("false", false)]
    fn bool(#[case] inp: &str, #[case] exp: bool) {
        assert_eq!(jbool(inp), Ok(("", Bool(exp))));
    }

    #[rstest]
    #[case("123.456", 123.456)]
    #[case("-123.456", -123.456)]
    #[case("0.456", 0.456)]
    #[case("123", 123.0)]
    #[case("1.6e12", 1.6e12)]
    #[case("1.6e+12", 1.6e12)]
    #[case("1.6e-12", 1.6e-12)]
    fn num(#[case] inp: &str, #[case] exp: f64) {
        assert_eq!(jnum(inp), Ok(("", Num(exp))));
    }

    #[rstest]
    #[case(r#""""#, "")]
    #[case(r#""hello""#, "hello")]
    #[case(r#""hello\nworld""#, "hello\nworld")]
    #[case(r#""hello\u0020world""#, "hello world")]
    #[case(r#""hello\u0020world\u0020""#, "hello world ")]
    fn str(#[case] inp: &str, #[case] exp: &str) {
        assert_eq!(jstr(inp), Ok(("", Str(exp.to_string()))));
    }

    #[rstest]
    #[case(r#"[]"#, vec![])]
    #[case(r#"["hello", "world"]"#, vec![Str("hello".to_string()), Str("world".to_string())])]
    #[case(r#"[1, 2, 3]"#, vec![Num(1.0), Num(2.0), Num(3.0)])]
    #[case(r#"[true, false]"#, vec![Bool(true), Bool(false)])]
    #[case(r#"[null, null]"#, vec![Null, Null])]
    #[case(r#"[{}, {}]"#, vec![Object(vec![]), Object(vec![])])]
    #[case(r#"[[], []]"#, vec![Array(vec![]), Array(vec![])])]
    #[case(r#"[1, "foo", null, true, {}, []]"#, vec![Num(1.0), Str("foo".to_string()), Null, Bool(true), Object(vec![]), Array(vec![])])]
    fn arr(#[case] inp: &str, #[case] exp: Vec<JsonValue>) {
        assert_eq!(jarr(inp), Ok(("", Array(exp))));
    }

    #[rstest]
    #[case(r#"{}"#, vec![])]
    #[case(r#"{"hello": "world"}"#, vec![("hello".to_string(), Str("world".to_string()))])]
    #[case(r#"{"hello": "world", "foo": "bar"}"#, vec![("hello".to_string(), Str("world".to_string())), ("foo".to_string(), Str("bar".to_string()))])]
    #[case(
        r#"{"pi": 3.14, "hello": "world!", "arr": [], "obj": {}, "null": null, "true": true, "false": false }"#,
        vec![
            ("pi".to_string(), Num(3.14)),
            ("hello".to_string(), Str("world!".to_string())),
            ("arr".to_string(), Array(vec![])),
            ("obj".to_string(), Object(vec![])),
            ("null".to_string(), Null),
            ("true".to_string(), Bool(true)),
            ("false".to_string(), Bool(false))
        ]
    )]
    fn obj(#[case] inp: &str, #[case] exp: Vec<(String, JsonValue)>) {
        assert_eq!(jobj(inp), Ok(("", Object(exp))));
    }

    #[rstest]
    #[case(r#"null"#, Null)]
    #[case(r#"true"#, Bool(true))]
    #[case(r#"false"#, Bool(false))]
    #[case(r#"123.456"#, Num(123.456))]
    #[case(r#""hello""#, Str("hello".to_string()))]
    #[case(r#"[]"#, Array(vec![]))]
    #[case(r#"{}"#, Object(vec![]))]
    #[case(r#"[1, "foo", null, true, {}, []]"#, Array(vec![Num(1.0), Str("foo".to_string()), Null, Bool(true), Object(vec![]), Array(vec![]) ]))]
    fn value(#[case] inp: &str, #[case] exp: JsonValue) {
        assert_eq!(jvalue(inp), Ok(("", exp)));
    }
}
