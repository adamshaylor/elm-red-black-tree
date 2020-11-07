module RedBlackTree exposing (..)

type Color
  = Red
  | Black

type Tree comparable
  = Empty
  | Node Color (Tree comparable) comparable (Tree comparable)

empty : Tree comparable
empty = Empty

singleton : comparable -> Tree comparable
singleton value = Node Black Empty value Empty

member : comparable -> Tree comparable -> Bool
member searchValue tree =
  case tree of
    Empty ->
      False
    (Node _ left nodeValue right) ->
      case compare searchValue nodeValue of
        EQ -> True
        LT -> member searchValue left
        GT -> member searchValue right

insert : comparable -> Tree comparable -> Tree comparable
insert newValue tree =
  let
    ins subtree =
      case subtree of
        Empty ->
          Node Red Empty newValue Empty
        Node color left nodeValue right ->
          case compare newValue nodeValue of
            LT -> balance (Node color (ins left) nodeValue right)
            EQ -> subtree
            GT -> balance (Node color left nodeValue (ins right))
  in
    makeBlack (ins tree)

makeBlack : Tree comparable -> Tree comparable
makeBlack tree =
  case tree of
    Empty -> Empty
    Node _ left value right -> Node Black left value right

-- Adapted from https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf
balance : Tree comparable -> Tree comparable
balance tree =
  case tree of
    Empty -> Empty
    Node Black (Node Red (Node Red a x b) y c) z d -> Node Red (Node Black a x b) y (Node Black c z d)
    Node Black (Node Red a x (Node Red b y c)) z d -> Node Red (Node Black a x b) y (Node Black c z d)
    Node Black a x (Node Red (Node Red b y c) z d) -> Node Red (Node Black a x b) y (Node Black c z d)
    Node Black a x (Node Red b y (Node Red c z d)) -> Node Red (Node Black a x b) y (Node Black c z d)
    Node color a x b -> Node color a x b
