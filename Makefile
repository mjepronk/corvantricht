site:
	stack build
	# stack exec -- site rebuild
	stack exec -- site build

watch: site
	stack exec -- site watch
