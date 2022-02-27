npx spago build
stack exec --stack-yaml=../stack-purs0.14.7.yaml -- purs-tsd-gen -d output
npx tsc --strict user.ts
