compile: \
	main.js

main.js: Main.elm
	elm-make Main.elm --output main.js

.PHONY: compile
