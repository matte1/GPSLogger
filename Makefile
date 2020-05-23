ormolu:
	git ls-files "./*.hs" | xargs ormolu --mode 'inplace'

hlint:
	hlint -g
