module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser

import Html exposing (Html, button, div, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput, onSubmit)

import Svg exposing (Svg, svg, line, circle)
import Svg.Attributes exposing
  ( width
  , height
  , viewBox
  , style
  , fill
  , stroke
  , strokeWidth
  , x
  , y
  , cx
  , cy
  , r
  , x1
  , y1
  , x2
  , y2
  , alignmentBaseline
  , textAnchor
  , fontSize
  , fontWeight
  )

import RedBlackTree exposing (Color(..), Tree(..))
import Html.Events exposing (onInput)
import Html exposing (form)


-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { tree : Tree Int
  , nodeValue: String
  }


init : Model
init =
  Model RedBlackTree.empty ""



-- UPDATE


type Msg
  = UpdateNodeValue String
  | Insert

update : Msg -> Model -> Model
update msg model =
  let
    maybeNodeInt = String.toInt model.nodeValue
  in
    case (msg, maybeNodeInt) of
      (UpdateNodeValue newValue, _) ->
        { model | nodeValue = newValue }
      (Insert, Just newInt) ->
        Model (RedBlackTree.insert newInt model.tree) ""
      (Insert, Nothing) ->
        { model | nodeValue = "INVALID" }


-- VIEW

nodeSize : Float
nodeSize = 2.5

nodeFontSize : Float
nodeFontSize = 2

vertexThickness : Float
vertexThickness = 0.5

parentChildSpacing : Float
parentChildSpacing = 10

svgNode : Color -> Int -> List (Svg msg)
svgNode colorType nodeValue =
  let
      backgroundColor = case colorType of
        Red -> "red"
        Black -> "black"
      textColor = case colorType of
        Red -> "black"
        Black -> "white"
  in
    [ circle
        [ cx "0"
        , cy "0"
        , r (String.fromFloat nodeSize)
        , fill backgroundColor
        ]
        []
    , Svg.text_
        [ x "0"
        , y "0"
        , alignmentBaseline "middle"
        , textAnchor "middle"
        , fontSize (String.fromFloat nodeFontSize)
        , fontWeight "bold"
        , fill textColor
        ]
        [ Svg.text (String.fromInt nodeValue)
        ]
    ]

type BranchDirection
  = LeftBranch
  | RightBranch

svgBranch : BranchDirection -> Tree Int -> List (Svg msg)
svgBranch direction childNode =
  let
    xValue = case direction of
      LeftBranch -> "-25%"
      RightBranch -> "25%"
  in
    [ line
        [ x1 "0"
        , y1 "0"
        , x2 xValue
        , y2 (String.fromFloat parentChildSpacing)
        , stroke "black"
        , strokeWidth (String.fromFloat vertexThickness)
        ]
        []
    , svg
        [ x xValue
        , y (String.fromFloat parentChildSpacing)
        , width "50%"
        , height "50%"
        , style "overflow: visible;"
        ]
        (svgSubTree childNode)
    ]

svgSubTree : Tree Int -> List (Svg msg)
svgSubTree rbTree =
  case rbTree of
    Empty ->
      []
    Node color Empty value Empty ->
      svgNode color value
    Node color leftNode value Empty ->
      (svgBranch LeftBranch leftNode)
      ++ (svgNode color value)
    Node color Empty value rightNode ->
      (svgBranch RightBranch rightNode)
      ++ (svgNode color value)
    Node color leftNode value rightNode ->
      (svgBranch LeftBranch leftNode)
      ++ (svgBranch RightBranch rightNode)
      ++ (svgNode color value)

view : Model -> Html Msg
view model =
  div
    []
    [ form
      [ onSubmit Insert ]
      [ input
          [ type_ "text"
          , value model.nodeValue
          , onInput UpdateNodeValue
          ]
          []
      , button [ type_ "submit" ] [ Html.text "Insert" ]
      ]
    , svg
      [ width "1000"
      , height "1000"
      , viewBox ("-50 " ++ (String.fromFloat -nodeSize) ++ " 100 100")
      , style "font-family: sans-serif; width: 100%;"
      ]
      (svgSubTree model.tree)
    ]
