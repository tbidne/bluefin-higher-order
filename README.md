# bluefin higher order

Trying to implement

```haskell
withArgs :: forall a. [String] -> Eff es a -> Eff es a
```

in [`bluefin`](https://hackage.haskell.org/package/bluefin).
