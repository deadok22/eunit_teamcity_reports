OUTPUT_DIR=out/production/eunit_teamcity

run_tests: build
	erl -pa $(OUTPUT_DIR) -run -eval "eunit:test([eunit_teamcity], [verbose])" -s init stop -noshell 
build:
	mkdir -p $(OUTPUT_DIR)
	erlc -o $(OUTPUT_DIR) -I src/ src/*.erl tests/*.erl 
clean:
	rm -rf out
