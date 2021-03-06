module App.VilleinPanel where

import Prelude
import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Pux.Html as Html
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Pux.CSS (style, position, left, bottom,
                absolute, em, transform, marginTop, marginLeft, rotate, deg,
                width, height, border, solid, black, whitesmoke, px)
import Pux.Html (Html, span, input, text)
import Pux.Html.Attributes (offset, type_, value, checked)
import Pux.Html.Events (onClick, onChange, FormEvent)

newtype Coord = Coord
  { x :: Int
  , y :: Int
  }

derive instance eqCoord :: Eq Coord
derive instance ordCoord :: Ord Coord
derive instance genericCoord :: Generic Coord
instance showCoord :: Show Coord where
  show = gShow

data Direction = North | East

derive instance eqDir :: Eq Direction
derive instance ordDir :: Ord Direction
derive instance genericDir :: Generic Direction
instance showDir :: Show Direction where
  show = gShow

newtype Edge = Edge
  { coord :: Coord
  , dir :: Direction
  }

derive instance eqEdge :: Eq Edge
derive instance ordEdge :: Ord Edge
derive instance genericEdge :: Generic Edge
instance showEdge :: Show Edge where
  show = gShow

data Action = Toggle Edge
            | SetToNum Int
            | NumberChange FormEvent
            | Reset

type Edges = Map Edge Boolean

type State =
  { edges :: Edges
  , number :: Int
  }

gridOfSize :: Int -> Edges
gridOfSize n = Map.fromFoldable <<< Array.concat $ do
  x <- Array.range (-n) n
  y <- Array.range (-n) n
  let c = Coord { x: x, y: y}
      e1 = Edge {coord: c, dir: North}
      e2 = Edge {coord: c, dir: East}
  pure $ [Tuple e1 false, Tuple e2 false]

init :: State
init = { edges: gridOfSize 2, number: 0}

mkEdge :: Int -> Int -> Direction -> Coord -> Edge
mkEdge dx dy dir (Coord {x: x, y: y}) =
  Edge { coord: Coord {x: x + dx, y: y + dy}
       , dir: dir
       }

digit1 :: Coord -> Array Edge
digit1 c = [mkEdge (-1) 0 East c]

digit2 :: Coord -> Array Edge
digit2 c =
  [ mkEdge 0 0 North c
  , mkEdge 0 0 East c
  ]

digit3 :: Coord -> Array Edge
digit3 c =
  [ mkEdge (-1) 0 East c
  , mkEdge 0 (-1) North c
  , mkEdge 0 0 North c
  ]

digit :: Tuple Int Coord -> Array Edge
digit (Tuple i c) =
  case i of
    0 -> []
    1 -> digit1 c
    2 -> digit2 c
    3 -> digit3 c
    _ -> []

coords :: Array Coord
coords = [ Coord {x: 0, y: 0} -- ones
         , Coord {x: -1, y: 1} -- fours
         , Coord {x: 1, y: 1} -- sixteens
         , Coord {x: 1, y: -1} -- sixty-fours
         ]

-- Produces digits in reverse order, suitable to zip with coords
toBase :: Int -> Int -> Array Int
toBase base n = unfoldr f n
  where f 0 = Nothing
        f x = Just (Tuple (x `mod` base) (x `div` base))

toEdges :: Int -> Map Edge Boolean
toEdges n = Map.unionWith (||) activeEdges init.edges
  where activeEdges = Map.fromFoldable (map (\x -> Tuple x true) (Array.concatMap digit (Array.zip digits coords)))
        digits = toBase 4 n

update :: Action -> State -> State
update (Toggle edge) state = state { edges = Map.update (Just <<< not) edge state.edges }
update (NumberChange ev) state =
  case (Int.fromString ev.target.value) of
    Just n -> update (SetToNum n) state
    Nothing -> state
update (SetToNum n) state = state { number = n, edges = toEdges n}
update Reset state = init

view :: State -> Html Action
view state =
  Html.div
    []
    [ input [ type_ "text", value (show state.number), onChange NumberChange] []
    , Html.div [ ] [ text (show (toBase 4 state.number)) ]
    , Html.div [ leftMarg ]
      [ Html.div [ turned ] (map connection <<< Array.fromFoldable <<< Map.toList $ state.edges)
      ]
    ]
    where leftMarg = style $ do
            marginLeft (-10.0 # em)
            marginTop (10.0 # em)
            width (10.0 # em)
            height (10.0 # em)
          turned = style $ do
            transform $ rotate (45.0 # deg)


center :: { x :: Int, y :: Int}
center = { x: 10, y: 10 }

getPosition :: Edge -> {x :: Number, y :: Number}
getPosition (Edge {dir: dir, coord: Coord coord}) = {x: x, y: y}
  where dx = if dir == East then 0.0 else 0.0
        dy = if dir == North then 0.0 else 0.0
        x = 2.0 * (dx + toNumber (coord.x + center.x))
        y = 2.0 * (dy + toNumber (coord.y + center.y))

connection :: Tuple Edge Boolean -> Html Action
connection (Tuple e@(Edge {dir: dir}) b) =
  -- input [ type_ "checkbox", checked b, onClick (const (Toggle e)), pos ] []
  Html.div [ styled ] []
  where styled = style $ do
          position absolute
          case dir of
            North -> height (2.0 # em)
            East -> width (2.0 # em)
          let pos = getPosition e
          border solid (1.0 # px) (if b then black else whitesmoke)
          left (pos.x # em)
          bottom (pos.y # em)
