build :
	cabal v2-build

doctest :
	perl -i -e 'while (<ARGV>) { print unless /package-id base-compat-\d+(\.\d+)*/; }' .ghc.environment.*
	doctest src

haddock :
	cabal v2-haddock --haddock-hyperlink-source
