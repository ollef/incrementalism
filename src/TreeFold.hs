module TreeFold where

treeFold :: Monoid a => [a] -> a
treeFold [] = mempty
treeFold [a] = a
treeFold as@(_:_:_) = treeFold $ pairEmUp as
  where
    pairEmUp (a:b:xs) = (a <> b) : pairEmUp xs
    pairEmUp xs = xs

treeFoldMap :: Monoid m => (a -> m) -> [a] -> m
treeFoldMap f = treeFold . Prelude.map f
