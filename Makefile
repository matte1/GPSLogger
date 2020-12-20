pipeline:
	python3 scripts/garmin/fitdump.py --input_folder data/garmin/fit/ --output_folder data/garmin/jsons/ 

run:
	stack build && stack exec LifeOfMatt

elm:
	elm reactor

ormolu:
	git ls-files "./*.hs" | xargs ormolu --mode 'inplace'

hlint:
	hlint -g
