build:
	stack build
	stack exec -- site build

rebuild:
	stack build
	stack exec -- site rebuild

watch: build
	stack exec -- site watch

deploy: build
	stack exec -- site deploy
