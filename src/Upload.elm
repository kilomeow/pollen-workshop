module Upload exposing (..)

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
  { hover : Bool
  , files : List File
  , upload_error : Maybe Http.Error
  }


init : () -> (Model, Cmd Msg)
init _ =
  (Model False [] Nothing, Cmd.none)



-- UPDATE


type Msg
  = Pick
  | DragEnter
  | DragLeave
  | GotFiles File (List File)
  | Upload
  | Uploaded (Result Http.Error ())


upload : File.File -> Cmd Msg
upload file =
  Http.request
    { method = "PUT"
    , headers = []
    , url = "/upload?filename=" ++ File.name file
    , body = Http.fileBody file
    , expect = Http.expectWhatever Uploaded
    , timeout = Nothing
    , tracker = Nothing
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pick ->
      ( model
      , Select.files ["text/*"] GotFiles
      )

    DragEnter ->
      ( { model | hover = True }
      , Cmd.none
      )

    DragLeave ->
      ( { model | hover = False }
      , Cmd.none
      )

    GotFiles file files ->
      ( { model
            | files = file :: files
            , hover = False
        }
      , Cmd.none
      )

    Uploaded result ->
        let
            new_model = case result of
                Ok value ->  { model | files = [] , upload_error = Nothing }
                Err error -> { model | upload_error = Just error }
        in ( new_model , Cmd.none )

    Upload ->
        (model, model.files |> List.map upload |> Cmd.batch )



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
    [ style "border" (if model.hover then "6px dashed purple" else "6px dashed #ccc")
    , style "border-radius" "20px"
    , style "width" "480px"
    , style "height" "100px"
    , style "margin" "100px auto"
    , style "padding" "20px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "align-items" "center"
    , hijackOn "dragenter" (D.succeed DragEnter)
    , hijackOn "dragover" (D.succeed DragEnter)
    , hijackOn "dragleave" (D.succeed DragLeave)
    , hijackOn "drop" dropDecoder
    ]
    [ button [ onClick Pick ] [ text "Add Files" ]
    , span [ style "color" "#ccc" ] ( model.files |> List.map (File.name >> text) )
    , span [ style "color" "#ccc" ] [ uploadStatus model.upload_error |> text ]
    , button [ onClick Upload ] [text "Upload!"]
    ]


dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)


hijack : msg -> (msg, Bool)
hijack msg =
  (msg, True)

