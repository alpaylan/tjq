mod error;

use error::*;
mod filter;
use filter::*;
mod json;
use json::*;
mod shape;
use shape::*;

fn parse(input: &str) -> Filter {
    todo!()
}

fn main() {
    let filter = Filter::Pipe(
        Box::new(Filter::ArrayIterator),
        Box::new(Filter::BinOp(
            Box::new(Filter::ObjIndex("age".to_string())),
            BinOp::Mul,
            Box::new(Filter::Number(30.0)),
        )),
    );

    let json = Json::Array(vec![
        Json::Object(vec![
            (
                "name".to_string(),
                Json::Object(vec![("a".to_string(), Json::String("John".to_string()))]),
            ),
            ("age".to_string(), Json::String("alp".to_string())),
        ]),
        Json::Object(vec![
            ("name".to_string(), Json::String("Jane".to_string())),
            ("age".to_string(), Json::Number(30.0)),
        ]),
    ]);

    println!("Input: {}", json);
    println!("Filter: {}", filter);

    let results = Filter::filter(&json, &filter, &builtin_filters(), &mut Default::default());

    for result in results {
        match result {
            Ok(result) => {
                println!("{result}");
            }
            Err(err) => {
                println!("{err}");
            }
        }
    }

    let (s, results) = Shape::new(&filter, &builtin_filters());

    println!("Shape: {}", s);
    println!(
        "Result shapes: {}",
        results
            .iter()
            .map(Shape::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    );
    // Check internal type mismatches in the shape
    println!("Running internal type checking...");
    let m = s.check_self(vec![]);
    match m {
        Some(m) => println!("{m}"),
        None => {
            let m = s.check(json, vec![]);

            match m {
                Some(m) => println!("{m}"),
                None => println!("The input conforms to the inferred shape"),
            }
        }
    }
}
