GHC_FLAGS+=-O -i../src

example: *.hs ../src/FRP/Rawe.hs ../src/FRP/Rawe/*.hs
	ghc $(GHC_FLAGS) --make -o example Main.hs
