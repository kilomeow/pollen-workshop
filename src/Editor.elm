module Editor exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Http


-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { name : String
  , text : String
  , upload_error : Maybe Http.Error
  }


init : () -> (Model, Cmd Msg)
init _ =
  (Model "" "" Nothing, Cmd.none)



-- UPDATE


type Msg
  = TextUpdate String
  | NameUpdate String
  | Upload
  | Uploaded (Result Http.Error ())


upload : String -> String -> Cmd Msg
upload name text =
  Http.request
    { method = "PUT"
    , headers = []
    , url = "/upload?filename=" ++ name ++ ".pm"
    , body = (Http.stringBody "" text)
    , expect = Http.expectWhatever Uploaded
    , timeout = Nothing
    , tracker = Nothing
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TextUpdate text ->
        ({ model | text = text } , Cmd.none)

    NameUpdate name ->
        ({ model | name = name } , Cmd.none)

    Uploaded result ->
        let
            new_model = case result of
                Ok value ->  { model | upload_error = Nothing }
                Err error -> { model | upload_error = Just error }
        in ( new_model , Cmd.none )

    Upload ->
        ( model, upload model.name model.text )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus resp ->
            "Bad Status"

        Http.BadBody resp ->
            "Bad Body"

        Http.BadUrl url ->
            "Malformed url: " ++ url


uploadStatus merr = 
    case merr of
        Just err -> errorToString err
        Nothing -> ""


view : Model -> Html Msg
view model =
  div
    [ style "border" "6px dashed #ccc"
    , style "border-radius" "20px"
    , style "max-width" "50em"
    , style "margin" "2em auto"
    , style "padding" "20px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "align-items" "center"
    ]
    [ input [ value model.name 
            , placeholder "Filename" 
            , onInput NameUpdate ]
            [ ]
    , input [ style "width" "100%"
            , style "height" "400px"
            , value model.text 
            , placeholder "Type your text here" 
            , onInput TextUpdate 
            , rows 10 ]
            [ ]
    , button [ onClick Upload ] [text "Upload!"]
    ]



