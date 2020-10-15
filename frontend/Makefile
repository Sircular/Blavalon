TOP_SRC="src/Main.elm"
OUTPUT="dist/elm.js"

build-dev:
	elm make $(TOP_SRC) --output $(OUTPUT)

build-debug:
	elm make $(TOP_SRC) --output $(OUTPUT) --debug

build-prod:
	elm make $(TOP_SRC) --output $(OUTPUT) --optimize
	uglifyjs $(OUTPUT) --compress\
		'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2'\
		--output=$(OUTPUT) && uglifyjs $(OUTPUT) --mangle --output=$(OUTPUT)

clean:
	rm -rf $(OUTPUT)

