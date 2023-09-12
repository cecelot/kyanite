#[cfg(test)]
mod parse {
    use std::fs::File;

    use kyanite::Program;

    #[test]
    fn access() {
        let program = Program::from(File::open("examples/access.kya").unwrap());
        assert_eq!(
            format!("{}", program),
            "println(((kyanite . fs) . read(\"examples/access.kya\", ((kyanite . fs) . O_READ))))\n"
        );
    }

    #[test]
    fn empty() {
        let program = Program::from(File::open("examples/function_empty.kya").unwrap());
        assert_eq!(format!("{}", program), "defn helloWorld(): Int {\n}\n");
    }

    #[test]
    fn hello() {
        let program = Program::from(File::open("examples/hello.kya").unwrap());
        assert_eq!(
            format!("{}", program),
            "println(\"hello world\", ((5 * 7) + 12))\n"
        );
    }

    #[test]
    fn precedence() {
        let program = Program::from("test.assert(5 * (7 + (10 - 3)) / 2 >= 30);".to_string());
        assert_eq!(
            format!("{}", program),
            "(test . assert((((5 * (7 + (10 - 3))) / 2) >= 30)))\n"
        )
    }
}
