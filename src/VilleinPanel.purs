module App.VilleinPanel where

import Prelude
import Pux.Html (Html, div, span, input, text)
import Pux.Html.Attributes (type_, value)
import Pux.Html.Events (onClick)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Generic (class Generic, gShow)

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
            | Reset

type Edges = Map Edge Boolean

type State = Edges

gridOfSize :: Int -> Edges
gridOfSize n = Map.fromFoldable <<< Array.concat $ do
  x <- Array.range (-n) n
  y <- Array.range (-n) n
  let c = Coord { x: x, y: y}
      e1 = Edge {coord: c, dir: North}
      e2 = Edge {coord: c, dir: East}
  pure $ [Tuple e1 false, Tuple e2 false]

init :: State
init = gridOfSize 2

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

update :: Action -> State -> State
update (Toggle edge) state = Map.update (Just <<< not) edge state
update Reset state = init

view :: State -> Html Action
view state =
  div
    []
    ((map checkbox <<< Array.fromFoldable <<< Map.toList $ state) <>
    [ span [] [ text (show state) ]
    ])


checkbox :: Tuple Edge Boolean -> Html Action
checkbox (Tuple e b) = input [ type_ "checkbox", value (show b), onClick (const (Toggle e))] []
