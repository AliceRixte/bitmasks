# flags

Bitmask utilities for boolean flags.



## Usage

Define your flag type as an enumeration:

```haskell
import Data.Word
import Data.Bitmask

data PizzaTopping =
    Cheese
  | Mushrooms
  | Pineapple
  | Ham
  deriving (Show, Eq, Bounded, Enum)

type PizzaMask = Bitmask PizzaTopping Word8
```

### Creating bitmasks


```haskell
-- A Margherita pizza (cheese only)
margherita :: PizzaMask
margherita = fromFlags [Cheese]

veggie :: PizzaMask
veggie = exceptFlags [Ham]

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


