# Session types in Haskell

The following repository collects the examples from the book chapter [Session Types with Linearity in Haskell](https://kar.kent.ac.uk/66632/1/RP_9788793519817C10.pdf) by Dominic Orchard and Nobuko Yoshida, published as part of the book [Behavioural Types: from Theory to Tools](https://www.riverpublishers.com/research_details.php?book_id=439#:~:text=As%20a%20survey%20of%20the,graduate%20students%20and%20software%20developers.) edited by Simon Gay and António Ravara, published by River 2017. 

Cite this chapter with

```
@article{orchard2017session,
  title={Session types with linearity in Haskell},
  author={Orchard, Dominic and Yoshida, Nobuko},
  journal={Behavioural Types: from Theory to Tools},
  pages={219},
  year={2017},
  publisher={River Publishers}
}
```

## Repostitory contents

Examples of capturing session types in Haskell due to a number of approaches

* pre-sessions - Provides the encoding of simple session types without linearity as described by this book chapter.

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
