npx spago build
stack exec --stack-yaml=../stack-purs0.15.14.yaml -- purs-tsd-gen -d output
npx tsc --strict --module es2015 user.mts
