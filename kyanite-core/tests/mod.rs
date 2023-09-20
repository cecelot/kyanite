#[cfg(test)]
mod parse {
    use std::fs::File;

    use kyanite::Program;

    #[test]
    fn access() {
        let program = Program::from_file(File::open("examples/access.kya").unwrap()).unwrap();
        assert_eq!(
            format!("{}", program),
            "defn main(): Void {\n\tprintln(((kyanite . fs) . read(\"examples/access.kya\", ((kyanite . fs) . O_READ))))\n}\n"
        );
    }

    #[test]
    fn empty() {
        let program =
            Program::from_file(File::open("examples/function_empty.kya").unwrap()).unwrap();
        assert_eq!(format!("{}", program), "defn helloWorld(): Int {\n}\n");
    }

    #[test]
    fn hello() {
        let program = Program::from_file(File::open("examples/hello.kya").unwrap()).unwrap();
        assert_eq!(
            format!("{}", program),
            "defn main(): Void {\n\tprintln(\"hello world\")\n}\n"
        );
    }

    #[test]
    fn precedence() {
        let program = Program::from_string(
            "defn main(): void {\n\ttest.assert(5 * (7 + (10 - 3)) / 2 >= 30);\n}".to_string(),
        )
        .unwrap();
        assert_eq!(
            format!("{}", program),
            "defn main(): Void {\n\t(test . assert((((5 * (7 + (10 - 3))) / 2) >= 30)))\n}\n"
        )
    }
}
