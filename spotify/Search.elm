module Search where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Events exposing (onChange, onEnter)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Signal exposing (message,forwardTo,Address)
import String
import Task

import Album
import Artist



-- MODEL


type alias Model =
    { query : String
    , queryType : QueryType
    , loading : Bool
    , albums : List Album.Model
    , artists : List Artist.Model
    , nextUrl : Maybe String
    , resultCount : Int
    }


init : (Model, Effects Action)
init =
  ( { query = ""
    , queryType = QueryAlbum
    , loading = False
    , albums = []
    , artists = []
    , nextUrl = Nothing
    , resultCount = 0
    }
  , Effects.none
  )

type alias Answer =
  { name : String
  , href : String
  }

type QueryType
  = QueryArtist
  | QueryAlbum

queryTypeFromString : String -> Maybe QueryType
queryTypeFromString queryTypeString =
  if queryTypeString == "artist"
    then Just QueryArtist
  else if queryTypeString == "album"
    then Just QueryAlbum
  else Nothing

queryTypeToString : QueryType -> String
queryTypeToString queryType =
  case queryType of
    QueryAlbum -> "album"
    QueryArtist -> "artist"


-- UPDATE


type Action
    = QueryChange String
    | QueryTypeChange String
    | Query
    | RegisterAnswers (Maybe (Int, Maybe String, List Answer))
    | AlbumAction Int Album.Action
    | ArtistAction Int Artist.Action
    | SetScroll Int


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    QueryChange newQuery ->
      ( { model | query <- newQuery }
      , Effects.none
      )

    QueryTypeChange newQueryTypeStr ->
      let
        mNewQueryType = queryTypeFromString newQueryTypeStr
        newQueryType = Maybe.withDefault model.queryType mNewQueryType
      in
        ( { model | queryType <- newQueryType }
        , Effects.none
        )

    Query ->
      ( { model | albums <- []
                , artists <- []
                , loading <- True }
      , search model
      )

    RegisterAnswers Nothing ->
      ({ model | loading <- False }, Effects.none)

    RegisterAnswers (Just (count, nextUrl, answers)) ->
      let
        model' = { model | loading <- False
                         , resultCount <- count
                         , nextUrl <- nextUrl }
      in
        case model.queryType of
          QueryAlbum ->
            let
              toAlbumEffects i answer =
                let (album, effects) = Album.init answer.name answer.href
                in  (album, Effects.map (AlbumAction (i + List.length model.albums)) effects)
              albumsEffects =
                List.indexedMap
                  toAlbumEffects
                  answers
              (albums, effects) = List.unzip albumsEffects
            in
              ( { model' | albums <- List.append model.albums albums
                         , artists <- [] }
              , Effects.batch effects
              )

          QueryArtist ->
            let
              toArtistEffects i answer =
                let (artist, effects) = Artist.init answer.name answer.href
                in  (artist, Effects.map (ArtistAction (i + List.length model.artists)) effects)
              artistsEffects =
                List.indexedMap
                  toArtistEffects
                  answers
              (artists, effects) = List.unzip artistsEffects
            in
              ( { model' | albums <- []
                        , artists <- List.append model.artists artists }
              , Effects.batch effects
              )

    AlbumAction i (Album.SelectArtist name) ->
      let
        model' =
          { model | albums <- []
                  , artists <- []
                  , query <- name
                  , queryType <- QueryArtist }
      in
        ( model', search model' )

    AlbumAction i albumAction ->
      let albumsEffects =
            List.indexedMap
              (\j album ->
                 if i == j
                   then Album.update albumAction album
                   else (album, Effects.none))
              model.albums
          (albums, effects) = List.unzip albumsEffects
      in
          ( { model | albums <- albums }
          , Effects.map (AlbumAction i) (Effects.batch effects) )

    ArtistAction i (Artist.SelectAlbum name) ->
      let
        model' =
          { model | albums <- []
          , artists <- []
          , query <- name
          , queryType <- QueryAlbum }
      in
        ( model', search model' )

    ArtistAction i artistAction ->
      let
        artistsEffects =
           List.indexedMap
             (\j artist ->
                if i == j
                  then Artist.update artistAction artist
                  else (artist, Effects.none))
             model.artists
        (artists , effects) = List.unzip artistsEffects
      in
        ( { model | artists <- artists }
        , Effects.map (ArtistAction i) (Effects.batch effects) )

    SetScroll scrollFromBottom ->
      if scrollFromBottom < 10 && not model.loading && isJust model.nextUrl
        then ( { model | loading <- True }, fetchMore model)
        else ( model, Effects.none )



-- VIEW


containerFluid =
  div [class "container-fluid"]


row =
  div [class "row"]


bootstrap =
  node "link"
    [ href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    , rel "stylesheet"
    ]
    []


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [("margin", "20px 0")] ]
    [ bootstrap
    , containerFluid
        [ inputForm address model
        , text (toString model.resultCount)
        , text " results"
        , resultsList address model
        ]
    ]


inputForm address model =
  let
    capitalize str =
      String.append
        (String.toUpper (String.left 1 str))
        (String.dropLeft 1 str)

    makeOption queryType =
      option
        [ value (queryTypeToString queryType) ]
        [ text (capitalize (queryTypeToString queryType)) ]

  in
    div
      [ class "form-group" ]
      [ input
        [ type' "text"
        , placeholder "Search for an album..."
        , value model.query
        , onChange address QueryChange
        , onEnter address Query
        ]
        []
      , select
          [ value (queryTypeToString model.queryType)
          , onChange address QueryTypeChange
          ]
          (List.map makeOption [QueryAlbum, QueryArtist])
      , button [onClick address Query] [text "Search"]
      , text (if model.loading then "loading..." else "")
      ]


resultsList address model =
  let
    toAlbumEntry i album =
      div
        [class "col-xs-3"]
        [Album.view (Signal.forwardTo address (AlbumAction i)) album]
    toArtistEntry i artist =
      div
        [class "col-xs-3"]
        [Artist.view (Signal.forwardTo address (ArtistAction i)) artist]
  in
    row (List.append
           (List.indexedMap toAlbumEntry model.albums)
           (List.indexedMap toArtistEntry model.artists))


-- EFFECTS


(=>) = (,)


search : Model -> Effects Action
search model =
  Http.get decodeAnswers (searchUrl model)
    |> Task.toMaybe
    |> Task.map RegisterAnswers
    |> Effects.task


searchUrl : Model -> String
searchUrl model =
  Http.url "https://api.spotify.com/v1/search"
    [ "q" => model.query
    , "type" => queryTypeToString model.queryType
    ]


fetchMore : Model -> Effects Action
fetchMore model =
  case model.nextUrl of
    Just nextUrl ->
      Http.get decodeAnswers nextUrl
        |> Task.toMaybe
        |> Task.map RegisterAnswers
        |> Effects.task
    Nothing ->
      Effects.none


decodeAnswers : Json.Decoder (Int, Maybe String, List Answer)
decodeAnswers =
  let
    album =
      Json.object2 Answer
        ("name" := Json.string)
        ("href" := Json.string)
    answers =
      Json.oneOf
        [ (Json.at ["albums", "items"] (Json.list album))
        , (Json.at ["artists", "items"] (Json.list album))
        ]
  in
    Json.object3 (,,)
      (Json.oneOf
        [ Json.at ["albums"] ("total" := Json.int)
        , Json.at ["artists"] ("total" := Json.int)
        ])
      (Json.oneOf
         [ Json.at ["albums"] ("next" := nullOr Json.string)
         , Json.at ["artists"] ("next" := nullOr Json.string)
         ])
      answers

nullOr : Json.Decoder a -> Json.Decoder (Maybe a)
nullOr decoder =
  Json.oneOf
    [ Json.null Nothing
    , Json.map Just decoder
    ]

isJust : Maybe a -> Bool
isJust m =
  case m of
    Just _ -> True
    Nothing -> False

isNothing : Maybe a -> Bool
isNothing = not << isJust
