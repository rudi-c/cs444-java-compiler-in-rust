all:
	cd ast && cargo build && cd .. && ln -s -f ast/target/ast joosc
