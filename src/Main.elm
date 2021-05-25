port module Main exposing (..)

import Browser
import Css.Global
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Html.Styled.Events as Event
import Json.Decode as D
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { draft : String
    , messages : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { draft = "", messages = [ "empezando por uno" ] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DraftChanged String
    | Send
    | Recv String



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged draft ->
            ( { model | draft = draft }
            , Cmd.none
            )

        Send ->
            ( { model | draft = "" }
            , sendMessage model.draft
            )

        Recv message ->
            ( { model | messages = model.messages ++ [ message ] }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Css.Global.global Tw.globalStyles
        , div
            [ css [ Tw.bg_green_200, Tw.p_8 ] ]
            [ Html.h1
                [ css [ Tw.text_3xl ] ]
                [ text "Echo Chat" ]
            , Html.ul [ css [ Tw.flex ] ]
                (List.map
                    (\msg -> Html.li [ css [ Tw.ml_3 ] ] [ text msg ])
                    model.messages
                )
            , Html.br [] []
            , Html.input
                [ Attr.type_ "text"
                , css [ Tw.border, Tw.border_indigo_600 ]
                , Attr.placeholder "Escribe pues"
                , Event.onInput DraftChanged
                , Event.on "keydown" (ifIsEnter Send)
                , Attr.value model.draft
                ]
                []
            , Html.button
                [ Event.onClick Send
                , css [ Tw.ml_3, Tw.border, Tw.border_black, Tw.bg_yellow_300, Tw.p_2, Tw.rounded]]
                [ text "Send" ]

            ]
        ]



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )
