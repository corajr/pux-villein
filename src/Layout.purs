module App.Layout where

import App.VilleinPanel as VilleinPanel
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

data Action
  = Child (VilleinPanel.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: VilleinPanel.State }

init :: State
init =
  { route: NotFound
  , count: VilleinPanel.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = VilleinPanel.update action state.count }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ VilleinPanel.view state.count
        NotFound -> NotFound.view state
    ]
