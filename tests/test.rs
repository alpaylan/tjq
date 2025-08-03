use assert_cmd::Command;
use serde_json::Value;
use std::{fs, path::Path};


pub fn tester(group: &str, name: &str) -> Vec<Value> {
    let case_dir  = Path::new("tests").join(group).join(name);
    let expr_path = case_dir.join("test.jq");
    let input_path = case_dir.join("input.json");

    let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME"))
        .expect("binary not found; check your Cargo.toml `package.name`");
    cmd.arg("--path")
        .arg(&expr_path)
        .arg("--input-path")
        .arg(&input_path);

    let output = cmd
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();
    let s = String::from_utf8(output).expect("stdout not valid UTF-8");

    // collect only valid JSON lines
    s.lines()
     .filter_map(|l| serde_json::from_str::<Value>(l.trim()).ok())
     .collect()
}



#[test]
fn test_short_test1() {
    // 1) invoke the tester
    let actual = tester("short", "test1");

    let expected_path = Path::new("tests")
        .join("short")
        .join("test1")
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}
#[test]
fn test_short_test2() {
    // 1) invoke the tester
    let name = String::from("test2");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_short_test3() {
    // 1) invoke the tester
    let name = String::from("test3");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_short_test4() {
    // 1) invoke the tester
    let name = String::from("test4");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_short_test5() {
    // 1) invoke the tester
    let name = String::from("test5");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}
#[test]
fn test_short_test6() {
    // 1) invoke the tester
    let name = String::from("test6");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}
#[test]
fn test_short_test7() {
    // 1) invoke the tester
    let name = String::from("test7");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}
#[test]
fn test_short_test8() {
    // 1) invoke the tester
    let name = String::from("test8");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}
#[test]
fn test_short_test9() {
    // 1) invoke the tester
    let name = String::from("test9");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}


#[test]
fn test_short_test10() {
    // 1) invoke the tester
    let name = String::from("test10");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}


#[test]
fn test_medium_test1() {
    // 1) invoke the tester
    let name = String::from("test1");
    let actual = tester("medium", &name);

    let expected_path = Path::new("tests")
        .join("medium")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}
#[test]
fn test_medium_test2() {
    // 1) invoke the tester
    let name = String::from("test2");
    let actual = tester("medium", &name);

    let expected_path = Path::new("tests")
        .join("medium")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_medium_test3() {
    // 1) invoke the tester
    let name = String::from("test3");
    let actual = tester("medium", &name);

    let expected_path = Path::new("tests")
        .join("medium")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_medium_test4() {
    // 1) invoke the tester
    let name = String::from("test4");
    let actual = tester("medium", &name);

    let expected_path = Path::new("tests")
        .join("medium")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_medium_test5() {
    // 1) invoke the tester
    let name = String::from("test5");
    let actual = tester("medium", &name);

    let expected_path = Path::new("tests")
        .join("medium")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}


#[test]
fn test_long_test1() {
    // 1) invoke the tester
    let name = String::from("test1");
    let actual = tester("long", &name);

    let expected_path = Path::new("tests")
        .join("long")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}
#[test]
fn test_long_test2() {
    // 1) invoke the tester
    let name = String::from("test2");
    let actual = tester("long", &name);

    let expected_path = Path::new("tests")
        .join("long")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_long_test3() {
    // 1) invoke the tester
    let name = String::from("test3");
    let actual = tester("long", &name);

    let expected_path = Path::new("tests")
        .join("long")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_long_test4() {
    // 1) invoke the tester
    let name = String::from("test4");
    let actual = tester("long", &name);

    let expected_path = Path::new("tests")
        .join("long")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

#[test]
fn test_long_test5() {
    // 1) invoke the tester
    let name = String::from("test5");
    let actual = tester("long", &name);

    let expected_path = Path::new("tests")
        .join("long")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}


#[test]
fn test_short_test11() {
    // 1) invoke the tester
    let name = String::from("test11");
    let actual = tester("short", &name);

    let expected_path = Path::new("tests")
        .join("short")
        .join(name)
        .join("out.json");
    let expected: Vec<Value> = serde_json::from_str(
        &fs::read_to_string(&expected_path)
            .expect("failed to read expected"),
    )
    .expect("expected file is not a JSON array");

    // 3) compare
    assert_eq!(actual, expected, "Mismatch in short/test1");
}

