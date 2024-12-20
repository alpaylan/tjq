mod filter;
use filter::*;
mod json;
use json::*;
mod shape;
use shape::*;

fn main() {
    let filter = Filter::Pipe(
        Box::new(Filter::Pipe(
            Box::new(Filter::ArrayIterator),
            Box::new(Filter::Comma(
                Box::new(Filter::ObjIndex("age".to_string())),
                Box::new(Filter::ObjIndex("name".to_string())),
            )),
        )),
        Box::new(Filter::Object(vec![(
            Filter::String("v".to_string()),
            Filter::ObjIndex("a".to_string()),
        )])),
    );

    let json = Json::Array(vec![
        Json::Object(vec![
            (
                "name".to_string(),
                Json::Object(vec![("a".to_string(), Json::String("John".to_string()))]),
            ),
            (
                "age".to_string(),
                Json::Object(vec![("a".to_string(), Json::Number(25.0))]),
            ),
        ]),
        Json::Object(vec![
            ("name".to_string(), Json::String("Jane".to_string())),
            ("age".to_string(), Json::Number(30.0)),
        ]),
    ]);

    println!("Input: {}", json);
    println!("Filter: {}", filter);

    let result = Filter::filter(&json, &filter);

    match result {
        Ok(results) => {
            if results.is_empty() {
                println!("null");
            }

            for result in results {
                println!("{}", result);
            }
        }
        Err(err) => {
            println!("error: {:?}", err);
        }
    }

    let s = Shape::new(&filter);

    println!("Shape: {}", s);

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
