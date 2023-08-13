{-# LANGUAGE NamedFieldPuns #-}

module Main where

data Color = Red | Black | DoubleBlack deriving (Show)

data Tree a = Empty | DoubleEmpty | Node {color :: Color, value :: a, left :: Tree a, right :: Tree a}
  deriving (Show)

empty :: Tree a
empty = Empty

-- Nodes are represented by alphabetic letters starting by a: a -> b -> c -> d
-- Values are represented by alphabetic letters starting by x: x -> y -> z (when we got more than 3 values it will start by w)
balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance
  Black
  ( Node
      { color = Red,
        value = x,
        left =
          ( Node
              { color = Red,
                value = y,
                left = a,
                right = b
              }
            ),
        right = c
      }
    )
  z
  d =
    Node
      { color = Red,
        value = y,
        left =
          ( Node
              { color = Black,
                value = x,
                left = a,
                right = b
              }
          ),
        right =
          ( Node
              { color = Black,
                value = z,
                left = c,
                right = d
              }
          )
      }
balance
  Black
  ( Node
      { color = Red,
        value = x,
        left = a,
        right =
          ( Node
              { color = Red,
                value = y,
                left = b,
                right = c
              }
            )
      }
    )
  z
  d =
    Node
      { color = Red,
        value = y,
        left =
          ( Node
              { color = Black,
                value = x,
                left = a,
                right = b
              }
          ),
        right =
          ( Node
              { color = Black,
                value = z,
                left = c,
                right = d
              }
          )
      }
balance
  Black
  a
  x
  ( Node
      { color = Red,
        value = y,
        left =
          ( Node
              { color = Red,
                value = z,
                left = b,
                right = c
              }
            ),
        right = d
      }
    ) =
    Node
      { color = Red,
        value = y,
        left =
          ( Node
              { color = Black,
                value = x,
                left = a,
                right = b
              }
          ),
        right =
          ( Node
              { color = Black,
                value = z,
                left = c,
                right = d
              }
          )
      }
balance
  Black
  a
  x
  ( Node
      { color = Red,
        value = y,
        left = b,
        right =
          ( Node
              { color = Red,
                value = z,
                left = c,
                right = d
              }
            )
      }
    ) =
    Node
      { color = Red,
        value = y,
        left =
          ( Node
              { color = Black,
                value = x,
                left = a,
                right = b
              }
          ),
        right =
          ( Node
              { color = Black,
                value = z,
                left = c,
                right = d
              }
          )
      }
balance
  DoubleBlack
  a
  x
  ( Node
      { color = Red,
        value = y,
        left =
          ( Node
              { color = Red,
                value = z,
                left = b,
                right = c
              }
            ),
        right = d
      }
    ) =
    Node
      { color = Black,
        value = y,
        left =
          ( Node
              { color = Black,
                value = x,
                left = a,
                right = b
              }
          ),
        right =
          ( Node
              { color = Black,
                value = z,
                left = c,
                right = d
              }
          )
      }
balance
  DoubleBlack
  ( Node
      { color = Red,
        value = x,
        left = a,
        right =
          ( Node
              { color = Red,
                value = y,
                left = b,
                right = c
              }
            )
      }
    )
  z
  d =
    Node
      { color = Black,
        value = y,
        left =
          ( Node
              { color = Black,
                value = x,
                left = a,
                right = b
              }
          ),
        right =
          ( Node
              { color = Black,
                value = z,
                left = c,
                right = d
              }
          )
      }
balance color a x b = Node {color, value = x, left = a, right = b}

insert :: Ord a => Tree a -> a -> Tree a
insert tree value = blacken (ins tree)
  where
    ins Empty = Node {color = Red, value = value, left = Empty, right = Empty}
    ins (Node {color, value = x, left = a, right = b})
      | value < x = balance color (ins a) x b
      | value == x = Node {color, value = x, left = a, right = b}
      | otherwise = balance color a x (ins b)

    blacken
      ( Node
          { color = Red,
            value = x,
            left =
              ( Node
                  { color = Red,
                    value = y,
                    left = a,
                    right = b
                  }
                ),
            right = c
          }
        ) =
        Node
          { color = Black,
            value = y,
            left =
              ( Node
                  { color = Red,
                    value = x,
                    left = a,
                    right = b
                  }
              ),
            right = c
          }
    blacken
      ( Node
          { color = Red,
            value = x,
            left = a,
            right =
              ( Node
                  { color = Red,
                    value = y,
                    left = b,
                    right = c
                  }
                )
          }
        ) =
        Node
          { color = Black,
            value = x,
            left = a,
            right =
              ( Node
                  { color = Red,
                    value = y,
                    left = b,
                    right = c
                  }
              )
          }
    blacken tree' = tree'

rotate :: Color -> Tree a -> a -> Tree a -> Tree a
rotate
  Red
  (Node {color = DoubleBlack, left = a, value = x, right = b})
  y
  (Node {color = Black, left = c, value = z, right = d}) =
    balance Black (Node {color = Red, left = (Node {color = Black, left = a, value = x, right = b}), value = y, right = c}) z d
rotate
  Red
  DoubleEmpty
  y
  (Node {color = Black, left = c, value = z, right = d}) =
    balance Black (Node {color = Red, left = Empty, value = y, right = c}) z d
rotate
  Red
  (Node {color = Black, left = a, value = x, right = b})
  y
  (Node {color = DoubleBlack, left = c, value = z, right = d}) =
    balance Black a x (Node {color = Red, left = b, value = y, right = (Node {color = Black, left = c, value = z, right = d})})
rotate
  Red
  (Node {color = Black, left = a, value = x, right = b})
  y
  DoubleEmpty =
    balance Black a x (Node {color = Red, left = b, value = y, right = Empty})
rotate
  Black
  (Node {color = DoubleBlack, left = a, value = x, right = b})
  y
  (Node {color = Black, left = c, value = z, right = d}) =
    balance DoubleBlack (Node {color = Red, left = (Node {color = Black, left = a, value = x, right = b}), value = y, right = c}) z d
rotate
  Black
  DoubleEmpty
  y
  (Node {color = Black, left = c, value = z, right = d}) =
    balance DoubleBlack (Node {color = Red, left = Empty, value = y, right = c}) z d
rotate
  Black
  (Node {color = Black, left = a, value = x, right = b})
  y
  (Node {color = DoubleBlack, left = c, value = z, right = d}) =
    balance DoubleBlack a x (Node {color = Red, left = b, value = y, right = (Node {color = Black, left = c, value = z, right = d})})
rotate
  Black
  (Node {color = Black, left = a, value = x, right = b})
  y
  DoubleEmpty =
    balance DoubleBlack a x (Node {color = Red, left = b, value = y, right = Empty})
rotate
  Black
  (Node {color = DoubleBlack, left = a, value = w, right = b})
  x
  (Node {color = Red, left = (Node {color = Black, left = c, value = y, right = d}), value = z, right = e}) =
    Node
      { color = Black,
        left = balance Black (Node {color = Red, left = (Node {color = Black, left = a, value = w, right = b}), value = x, right = c}) y d,
        value = z,
        right = e
      }
rotate
  Black
  DoubleEmpty
  x
  (Node {color = Red, left = (Node {color = Black, left = c, value = y, right = d}), value = z, right = e}) =
    Node {color = Black, left = balance Black (Node {color = Red, left = Empty, value = x, right = c}) y d, value = z, right = e}
rotate
  Black
  (Node {color = Red, left = a, value = w, right = (Node {color = Black, left = b, value = x, right = c})})
  y
  (Node {color = DoubleBlack, left = d, value = z, right = e}) =
    Node
      { color = Black,
        left = a,
        value = w,
        right = balance Black b x (Node {color = Red, left = c, value = y, right = (Node {color = Black, left = d, value = z, right = e})})
      }
rotate
  Black
  (Node {color = Red, left = a, value = w, right = (Node {color = Black, left = b, value = x, right = c})})
  y
  DoubleEmpty =
    Node {color = Black, left = a, value = w, right = balance Black b x (Node {color = Red, left = c, value = y, right = Empty})}
rotate color a x b = Node {color, left = a, value = x, right = b}

minDel :: Ord a => Tree a -> (a, Tree a)
minDel (Node {color = Red, left = Empty, value = x, right = Empty}) = (x, Empty)
minDel (Node {color = Black, left = Empty, value = x, right = Empty}) = (x, DoubleEmpty)
minDel (Node {color = Black, left = Empty, value = x, right = (Node {color = Red, left = Empty, value = y, right = Empty})}) =
  (x, Node {color = Black, left = Empty, value = y, right = Empty})
minDel (Node {color, left, value, right}) = let (x, a) = minDel left in (x, rotate color a value right)

delete :: Ord a => Tree a -> a -> Tree a
delete tree value = del (redden tree)
  where
    del Empty = Empty
    del (Node {color = Red, left = Empty, value = x, right = Empty})
      | value == x = Empty
      | value /= x = Node {color = Red, left = Empty, value = x, right = Empty}
    del (Node {color = Black, left = Empty, value = x, right = Empty})
      | value == x = DoubleEmpty
      | value /= x = (Node {color = Black, left = Empty, value = x, right = Empty})
    del (Node {color = Black, left = (Node {color = Red, left = Empty, value = x, right = Empty}), value = y, right = Empty})
      | value < y = Node {color = Black, left = del (Node {color = Red, left = Empty, value = x, right = Empty}), value = y, right = Empty}
      | value == y = Node {color = Black, left = Empty, value = x, right = Empty}
      | value > y = Node {color = Black, left = (Node {color = Red, left = Empty, value = x, right = Empty}), value = y, right = Empty}
    del (Node {color, left = a, value = x, right = b})
      | value < x = rotate color (del a) x b
      | value == x = let (x', b') = minDel b in rotate color a x' b'
      | value > x = rotate color a x (del b)

    redden
      ( Node
          { color = Black,
            left = (Node {color = Black, left = a, value = x, right = b}),
            value = y,
            right = (Node {color = Black, left = c, value = z, right = d})
          }
        ) =
        Node
          { color = Red,
            left = (Node {color = Black, left = a, value = x, right = b}),
            value = y,
            right = (Node {color = Black, left = c, value = z, right = d})
          }
    redden t = t

search :: Ord a => Tree a -> a -> Maybe a
search Empty value = Nothing
search (Node {color = _, left, value = x, right}) value
  | value == x = Just x
  | value < x = search left value
  | value > x = search right value

main :: IO ()
main = print "Red-Black Tree :)"
