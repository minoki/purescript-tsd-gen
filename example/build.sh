npx spago build
stack exec --stack-yaml=../stack-purs0.14.5.yaml -- purs-tsd-gen -d output
npx tsc --strict user.ts
