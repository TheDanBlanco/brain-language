lint fmt:
	cargo fix --allow-dirty
	cargo fmt --all

coverage:
	CARGO_INCREMENTAL=0 RUSTFLAGS='-Cinstrument-coverage' cargo test
	mkdir -p target/coverage/html
	grcov . --binary-path ./target/debug/deps/ -s . -t html --branch --ignore-not-existing --ignore '../*' --ignore "/*" -o ./target/coverage/html
	find . -name '*.profraw' -delete
	open ./target/coverage/html/index.html

bootstrap:
	cargo install grcov