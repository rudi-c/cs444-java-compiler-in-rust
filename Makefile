all:
	cd emit && cargo build && cd .. && ln -s -f emit/target/emit joosc
