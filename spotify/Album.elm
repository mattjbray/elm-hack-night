module Album
  (Model, init, view, Action(SelectArtist), update)
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
  , artists : List String
  , images : List String
  }


init : String -> String -> (Model, Effects Action)
init name url =
  ( { name = name
    , url = url
    , artists = []
    , images = []
    }
  , Task.succeed FetchData |> Effects.task
  )


type Action
  = FetchData
  | RegisterData (Maybe Model)
  | SelectArtist String


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
  let
    artistLink name =
      a [ href "#"
        , onClick address (SelectArtist name)]
        [text name]
  in
    div
      [class "panel panel-info"]
      [ div
          [class "panel-heading"]
          [text "Album"]
      , div
          [ class "panel-body"
          , style [("height", "28rem")]
          ]
          ([ img
               [ src (Maybe.withDefault "" (List.head model.images))
               , class "img-thumbnail"
               , style [ ("width", "100%")
                       , ("height", "20rem")
                       ]
               ]
               []
           ]
          `List.append`
           [ text model.name
           , text " by "
           ]
          `List.append`
           List.map artistLink model.artists
          )
      ]


fetchData : Model -> (Model, Effects Action)
fetchData model =
  ( model
  , Http.get decodeAlbum model.url
      |> Task.toMaybe
      |> Task.map RegisterData
      |> Effects.task
  )


decodeAlbum : Json.Decoder Model
decodeAlbum =
  Json.object4 Model
    ("name" := Json.string)
    ("href" := Json.string)
    (Json.at ["artists"] (Json.list ("name" := Json.string)))
    (Json.at ["images"] (Json.list ("url" := Json.string)))
