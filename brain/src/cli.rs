pub struct Cli {
    pub file: String,
    pub debug: bool,
    pub help: bool,
    pub version: bool,
}

impl Cli {
    pub fn new() -> Self {
        Cli {
            file: "".to_string(),
            debug: false,
            help: false,
            version: false,
        }
    }

    pub fn parse(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        // we expect the cli to be called like:
        // brain <file> [--debug] [--help] [--version]
        // if no file is provided, we should return a usage message

        let args: Vec<String> = std::env::args().collect();
        if args.len() < 2 {
            return Err("Usage: brain [file] [--debug] [--help] --version]".into());
        }

        self.file = args[1].clone();

        for arg in args {
            match arg.as_str() {
                "--debug" => self.debug = true,
                "--help" => self.help = true,
                "--version" => self.version = true,
                _ => (),
            }
        }

        Ok(())
    }
}
