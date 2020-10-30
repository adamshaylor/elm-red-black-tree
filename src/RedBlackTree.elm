module RedBlackTree exposing (empty, singleton, member, insert)

type Color
  = Red
  | Black
  
type Branch comparable
  = Leaf
  | LeftBranch (Node comparable)
  | RightBranch (Node comparable)
  | LeftRightBranch (Node comparable) (Node comparable)

type Node comparable
  = Node Color comparable (Branch comparable)

type RedBlackTree comparable
  = EmptyTree
  | NonEmptyTree (Node comparable)

empty : RedBlackTree comparable
empty = EmptyTree

singleton : comparable -> RedBlackTree comparable
singleton value = NonEmptyTree (Node Black value Leaf)

memberOfNode : comparable -> Node comparable -> Bool
memberOfNode searchValue node =
  let
    (Node _ nodeValue branch) = node
    searchComparison = compare searchValue nodeValue
  in
    case (searchComparison, branch) of
      (EQ, _) -> True
      (_, Leaf) -> False
      (LT, (LeftBranch leftNode)) -> memberOfNode searchValue leftNode
      (LT, (RightBranch _)) -> False
      (LT, (LeftRightBranch leftNode _)) -> memberOfNode searchValue leftNode
      (GT, (LeftBranch _)) -> False
      (GT, (RightBranch rightNode)) -> memberOfNode searchValue rightNode
      (GT, (LeftRightBranch _ rightNode)) -> memberOfNode searchValue rightNode

member : comparable -> RedBlackTree comparable -> Bool
member searchValue tree =
  case tree of
    EmptyTree -> False
    NonEmptyTree root -> memberOfNode searchValue root

redLeafNode : comparable -> Node comparable
redLeafNode value = Node Red value Leaf

insertIntoNode : comparable -> Node comparable -> Node comparable
insertIntoNode newValue targetNode =
  let
    (Node c v targetBranch) = targetNode
    searchComparison = compare newValue v
  in
    case (searchComparison, targetBranch) of
      (EQ, _) ->
        targetNode
      (LT, Leaf) ->
        Node c v (LeftBranch (redLeafNode newValue))
      (LT, LeftBranch leftNode) ->
        Node c v (LeftBranch (insertIntoNode newValue leftNode))
      (LT, RightBranch rightNode) ->
        Node c v (LeftRightBranch (redLeafNode newValue) rightNode)
      (LT, LeftRightBranch leftNode rightNode) ->
        Node c v (LeftRightBranch (insertIntoNode newValue leftNode) rightNode)
      (GT, Leaf) ->
        Node c v (RightBranch (redLeafNode newValue))
      (GT, LeftBranch leftNode) ->
        Node c v (LeftRightBranch leftNode (redLeafNode newValue))
      (GT, RightBranch rightNode) ->
        Node c v (RightBranch (insertIntoNode newValue rightNode))
      (GT, LeftRightBranch leftNode rightNode) ->
        Node c v (LeftRightBranch leftNode (insertIntoNode newValue rightNode))

insert : comparable -> RedBlackTree comparable -> RedBlackTree comparable
insert newValue targetTree =
  case targetTree of
    EmptyTree -> singleton newValue
    NonEmptyTree root -> NonEmptyTree
      (insertIntoNode newValue root)
