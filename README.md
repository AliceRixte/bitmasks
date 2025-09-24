# Bitmasks

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/AliceRixte/bitmasks/LICENSE) [![Hackage](https://img.shields.io/hackage/v/bitmasks.svg)](https://hackage.haskell.org/package/bitmasks) [![Nightly](https://www.stackage.org/package/bitmasks/badge/nightly)](https://www.stackage.org/nightly/package/bitmasks) [![LTS](https://www.stackage.org/package/bitmasks/badge/lts)](https://www.stackage.org/lts/package/bitmasks)


Bitmasks for efficient storing of boolean flags.

## Alternatives

The [bitset](https://hackage.haskell.org/package/bitset) uses a similar implementation to this library. It is:

* Better written
* Probably a lot faster

than this library.

*But*, for now (2025-09-24) it is also abandoned, and it does not compile.

I made this library before knowing [bitset](https://hackage.haskell.org/package/bitset) existed. One day when I have the time I might try to dive into [bitset](https://hackage.haskell.org/package/bitset) and either:

* Update [bitset](https://hackage.haskell.org/package/bitset) and mark this library as stale
* Import the code of [bitset](https://hackage.haskell.org/package/bitset) in this library

## Usage

Define your flags as an enumeration:

```haskell
import Data.Word
import Data.Bitmask

data PizzaTopping =
    Cheese
  | Mushrooms
  | Pineapple
  | Ham
  deriving (Show, Eq, Bounded, Enum)

-- We only need 8 bits since there are only 4 toppings
type PizzaMask = Bitmask8 PizzaTopping
```

### Creating bitmasks

```haskell
-- A Margherita pizza (cheese only)
margherita :: PizzaMask
margherita = fromFlags [Cheese]

veggie :: PizzaMask
veggie = fromExceptFlags [Ham]

```

### Access and modify flags

Use `getFlag` to check if a pizza has a specific topping:

```haskell
>>> getFlag Cheese funghi
True
>>> getFlag Pineapple funghi
False
```

Add toppings to a pizza:

```haskell
>>> hawaiian = addFlags [Pineapple, Ham] margherita
>>> getFlags [Pineapple, Mushroom] hawaiian
True
```

Make any pizza vegetarian (bitwise AND):

```haskell
>>> veggieHawaiian = veggie .&. hawaiian
>>> getFlag Ham veggieHawaiian
```

Toggle (I have no idea what I'm talking about) the toppings :

```haskell
>>> funghi = flipFlags [Pineapple, Mushroom] veggieHawaiian
>>> toFlags funghi
[Cheese,Mushrooms]
```

Remove a topping:

```haskell
>>> margherita == deleteFlag Mushroom funghi
True
```

### Convert to lists

```haskell
-- Get all toppings as a list
>>> toFlags funghi
[Cheese,Mushrooms]
>>> toFlags hawaiian
[Cheese,Pineapple,Ham]

-- Convert to association lists
>>> toFlagsBool funghi
[(Cheese,True),(Mushrooms,True),(Pineapple,False),(Ham,False)]
```
