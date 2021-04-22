module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font 
import Element.Input as Input
import Html exposing (Html)
import Term exposing (Term, parse)
import Value exposing (evaluate)


main : Program () Model Message
main =
    Browser.sandbox { init = init, update = update, view = view }


type Action t
    = Clean
    | Failure
    | Success t


type alias Model =
    { input : String
    , term : Action Term
    , value : Action Term
    }


init : Model
init =
    { input = ""
    , term = Clean
    , value = Clean
    }


type Message
    = UpdateProgram String
    | Parse
    | Evaluate


update : Message -> Model -> Model
update message model =
    case message of
        UpdateProgram input ->
            { input = input, term = Clean, value = Clean }

        Parse ->
            parseInput model

        Evaluate ->
            evaluateTerm model


parseInput : Model -> Model
parseInput model =
    case parse model.input of
        Just term ->
            { model | term = Success term }

        Nothing ->
            { model | term = Failure }


evaluateTerm : Model -> Model
evaluateTerm model =
    case model.term of
        Success term ->
            case evaluate term of
                Just value ->
                    { model | value = Success value }

                _ ->
                    { model | value = Failure }

        _ ->
            { model | value = Failure }



-- Colors


darkColor : Color
darkColor =
    rgb255 0x40 0x42 0x54


lightColor : Color
lightColor =
    rgb255 0x0248 0x0248 0x0242


lightDarkColor : Color
lightDarkColor =
    rgb255 0x68 0x71 0x90


buttonStyle : List (Attribute msg)
buttonStyle =
    [ padding 10
    , Border.width 3
    , Border.rounded 6
    , Border.color lightDarkColor
    , Background.color darkColor
    , Font.color lightColor
    , mouseDown
        [ Background.color lightDarkColor
        , Border.color darkColor
        ]
    , mouseOver
        [ Background.color lightColor
        , Font.color darkColor
        ]
    , focused
        [ Background.color darkColor
        , Font.color lightColor
        , Border.color lightDarkColor
        ]
    ]


view : Model -> Html Message
view model =
    layout [ padding 50, Background.color lightColor, Font.color darkColor, Font.size 32 ] <|
        column
            [ spacing 20
            , width <| maximum 1200 fill
            , centerX
            ]
            [ viewInput model
            , row
                [ spacing 20
                , alignRight
                ]
                [ viewParseButton model
                , viewEvaluateButton model
                ]
            , viewParsedTerm model
            , viewValue model
            ]


viewInput : Model -> Element Message
viewInput model =
    Input.multiline
        [ height <| px 300
        , Border.rounded 2
        , Border.width 2
        , Border.color <| lightDarkColor
        , Font.color <| lightColor
        , Background.color <| darkColor
        , focused
            [ Background.color darkColor
            , Font.color lightColor
            , Border.color lightDarkColor
            ]
        ]
        { onChange = UpdateProgram
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [] <| text "Program"
        , spellcheck = False
        }


viewParseButton : Model -> Element Message
viewParseButton _ =
    Input.button buttonStyle
        { onPress = Just Parse, label = text "Parse" }


viewEvaluateButton : Model -> Element Message
viewEvaluateButton _ =
    Input.button buttonStyle
        { onPress = Just Evaluate, label = text "Evaluate" }


success : List (Attr decorative msg)
success =
    [ Font.color <| darkColor ]


failure : List (Attr decorative msg)
failure =
    [ Font.color <| rgb255 0x0255 0x00 0x00 ]


viewParsedTerm : Model -> Element Message
viewParsedTerm model =
    case model.term of
        Clean ->
            paragraph [] [ text "" ]

        Success term ->
            paragraph success [ text ("Parsed: " ++ Term.toString term) ]

        Failure ->
            paragraph failure [ text "Unable to parse" ]


viewValue : Model -> Element Message
viewValue model =
    case model.value of
        Clean ->
            paragraph [] [ text "" ]

        Success value ->
                paragraph success [ text ("Parsed: " ++ Term.toString value) ]
                -- Value.Number n ->
                --     paragraph success [ text ("Value: " ++ String.fromInt n) ]

                -- Value.Boolean True ->
                --     paragraph success [ text "Value: yes " ]

                -- Value.Boolean False ->
                --     paragraph success [ text "Value: no " ]

        Failure ->
            paragraph failure [ text "Unable to evaluate" ]
