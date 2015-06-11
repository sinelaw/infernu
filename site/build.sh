ghc --make -threaded site.hs
./site clean && PATH=$PATH:node_modules/.bin ./site build
