# betty-book-haskell-sessions

* NT - Neubauer and Thiemann [2004] give an encoding of first-order single-channel session types with recursion; A
recreation of the code in their paper, with some bug fixes is include.

* PT - Using parameterised monads, Pucella and Tov [2008] provide multiple channels, recursion, and 
some building blocks for delegation, but require manual manipulation of a session type context;
`cabal install simple-sessions` first (http://hackage.haskell.org/package/simple-sessions)

* SE - Sackman and Eisenbach [2008] provide an alternate approach where session types are constructed via a value-level 
witness. Current implementation on Hackage is heavily bit-rotted (http://hackage.haskell.org/package/sessions)

* IY - Imai et al. [2010] extends Pucella-Tov with delegation and a more user-friendly approach to handling multiple channels; 
The latest version on Hackage (http://hackage.haskell.org/package/full-sessions) is not compatible with GHC 8 so I have
included a fixed version on the repository here. Run `cabal install` in the `ful-sessions-0.6.2.1` directory.

* OY - Orchard and Yoshida [2016] use an embedding of effect systems into Haskell via graded monads based on a formal 
encoding of session-typed π-calculus into PCF with an effect system. (https://github.com/dorchard/sessions-in-haskell)
The library is included here.

* GVinHS - Lindley and Morris [2016] provide a finally tagless embedding of the GV session-typed functional calculus into Haskell, building on a linear λ-calculus embedding due to Polakow [2016]. 
Download GVinHS first (https://github.com/jgbm/GVinHs)
