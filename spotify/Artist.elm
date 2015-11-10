module Artist
  (Model, init, view, Action(SelectAlbum), update)
  where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task


type alias Model =
  { name : String
  , url : String
  , images : List String
  }


init : String -> String -> (Model, Effects Action)
init name url =
  ( { name = name
    , url = url
    , images = []
    }
  , Task.succeed FetchData |> Effects.task
  )


type Action
  = FetchData
  | RegisterData (Maybe Model)
  | SelectAlbum String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    FetchData -> fetchData model

    RegisterData mNewModel ->
      ( Maybe.withDefault model mNewModel
      , Effects.none
      )


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [class "panel panel-info"]
    [ div
        [class "panel-heading"]
        [text "Artist"]
    , div
        [ class "panel-body"
        , style [("height", "28rem")]
        ]
        ([ img
           [ src (Maybe.withDefault "http://www.freelanceme.net/Images/default%20profile%20picture.png" (List.head model.images))
             , class "img-thumbnail"
             , style [ ("width", "100%")
                     , ("height", "20rem")
                     ]
             ]
             []
         ]
        `List.append`
         [ text model.name
         , p []
             [ a [ href "#"
                 , onClick address (SelectAlbum model.name)
                 ]
                 [ text "view albums"]
             ]
         ]
        )
    ]


fetchData : Model -> (Model, Effects Action)
fetchData model =
  ( model
  , Http.get decodeArtist model.url
      |> Task.toMaybe
      |> Task.map RegisterData
      |> Effects.task
  )


decodeArtist : Json.Decoder Model
decodeArtist =
  Json.object3 Model
    ("name" := Json.string)
    ("href" := Json.string)
    (Json.at ["images"] (Json.list ("url" := Json.string)))
