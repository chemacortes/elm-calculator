module Main exposing (Model, Msg, init, main, update, view)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as D
import Keyboard.Event as KE exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as KK
import Maybe exposing (withDefault)
import String



-- MODEL


type alias Model =
    { stack : List Float
    , currentNum : String
    , error : Maybe String
    , dirty : Bool
    }


initialModel : Model
initialModel =
    { stack = []
    , currentNum = "0"
    , error = Nothing
    , dirty = True
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown <|
        D.map HandleKeyboardEvent decodeKeyboardEvent



-- UPDATE


type Operator
    = Add
    | Sub
    | Mult
    | Div


operatorToString : Operator -> String
operatorToString operator =
    case operator of
        Div ->
            "÷"

        Mult ->
            "×"

        Sub ->
            "-"

        Add ->
            "+"


operatorFunction : Operator -> (Float -> Float -> Float)
operatorFunction operator =
    case operator of
        Add ->
            (+)

        Sub ->
            (-)

        Mult ->
            (*)

        Div ->
            (/)


type Msg
    = InputNumber Int
    | ClearAll
    | Clear
    | Back
    | Enter
    | InputOperator Operator
    | SetDecimal
    | SetSign
    | HandleKeyboardEvent KeyboardEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Enter ->
            let
                maybeNumber =
                    String.toFloat model.currentNum
            in
            case maybeNumber of
                Nothing ->
                    ( { model | error = Just "PARSE ERR" }, Cmd.none )

                Just num ->
                    ( { model
                        | stack = num :: model.stack
                        , dirty = True
                      }
                    , Cmd.none
                    )

        Clear ->
            ( { model
                | currentNum = "0"
                , dirty = True
              }
            , Cmd.none
            )

        ClearAll ->
            ( initialModel, Cmd.none )

        InputNumber num ->
            if model.dirty then
                ( { model
                    | currentNum = String.fromInt num
                    , dirty = False
                  }
                , Cmd.none
                )

            else
                ( { model
                    | currentNum = model.currentNum ++ String.fromInt num
                  }
                , Cmd.none
                )

        Back ->
            let
                newNum =
                    String.dropRight 1 model.currentNum
            in
            ( { model
                | currentNum =
                    if String.length newNum > 0 then
                        newNum

                    else
                        "0"
              }
            , Cmd.none
            )

        SetDecimal ->
            if String.contains "." model.currentNum then
                ( model, Cmd.none )

            else
                ( { model | currentNum = model.currentNum ++ "." }, Cmd.none )

        SetSign ->
            if model.currentNum == "0" then
                ( model, Cmd.none )

            else if String.startsWith "-" model.currentNum then
                ( { model | currentNum = String.dropLeft 1 model.currentNum }, Cmd.none )

            else
                ( { model | currentNum = "-" ++ model.currentNum }, Cmd.none )

        InputOperator operator ->
            case model.stack of
                [] ->
                    ( model, Cmd.none )

                x :: xs ->
                    let
                        op =
                            operatorFunction operator

                        num =
                            model.currentNum
                                |> String.toFloat
                                |> withDefault 0

                        newNum =
                            op num x
                    in
                    ( { model
                        | currentNum = String.fromFloat newNum
                        , stack = xs
                        , dirty = True
                      }
                    , Cmd.none
                    )

        HandleKeyboardEvent event ->
            if event.ctrlKey && event.shiftKey then
                case event.keyCode of
                    KK.Backspace ->
                        update ClearAll model

                    _ ->
                        ( model, Cmd.none )

            else if event.ctrlKey then
                case event.keyCode of
                    KK.Backspace ->
                        update Clear model

                    _ ->
                        ( model, Cmd.none )

            else
                case event.keyCode of
                    KK.Multiply ->
                        update (InputOperator Mult) model

                    KK.Divide ->
                        update (InputOperator Div) model

                    KK.Add ->
                        update (InputOperator Add) model

                    KK.Subtract ->
                        update (InputOperator Sub) model

                    KK.Decimal ->
                        update SetDecimal model

                    KK.Enter ->
                        update Enter model

                    KK.Backspace ->
                        update Back model

                    KK.NumpadZero ->
                        update (InputNumber 0) model

                    KK.NumpadOne ->
                        update (InputNumber 1) model

                    KK.NumpadTwo ->
                        update (InputNumber 2) model

                    KK.NumpadThree ->
                        update (InputNumber 3) model

                    KK.NumpadFour ->
                        update (InputNumber 4) model

                    KK.NumpadFive ->
                        update (InputNumber 5) model

                    KK.NumpadSix ->
                        update (InputNumber 6) model

                    KK.NumpadSeven ->
                        update (InputNumber 7) model

                    KK.NumpadEight ->
                        update (InputNumber 8) model

                    KK.NumpadNine ->
                        update (InputNumber 9) model

                    _ ->
                        ( model, Cmd.none )



-- VIEW


type Size
    = Single
    | Double
    | Triple


sizeToString : Size -> String
sizeToString size =
    case size of
        Single ->
            "single"

        Double ->
            "double"

        Triple ->
            "triple"


type Color
    = Yellow
    | Gray
    | White
    | Red


colorToString : Color -> String
colorToString color =
    case color of
        Yellow ->
            "bg-yellow"

        Gray ->
            "bg-gray"

        White ->
            "bg-white"

        Red ->
            "bg-red"


view : Model -> Browser.Document Msg
view model =
    let
        stack =
            List.reverse model.stack
                |> List.map (String.fromFloat >> text >> inputBox)
    in
    { title = "Elm Calculator (document)"
    , body =
        [ div []
            [ h1 [ class "h1" ] [ text "RPN Calculator" ]
            , div [ class "calculator" ]
                (stack
                    ++ [ case model.error of
                            Nothing ->
                                inputBox (text model.currentNum)

                            Just err ->
                                inputBox (span [ class "error" ] [ text err ])
                       , section
                       ]
                )
            ]
        ]
    }


section : Html Msg
section =
    div [ class "section" ]
        [ cell (onClick Back) Single Gray "←"
        , cell (onClick ClearAll) Single Red "C"
        , cell (onClick Clear) Single Gray "CE"
        , cellOp Div
        , cell (onClick <| InputNumber 7) Single White "7"
        , cell (onClick <| InputNumber 8) Single White "8"
        , cell (onClick <| InputNumber 9) Single White "9"
        , cellOp Mult
        , cell (onClick <| InputNumber 4) Single White "4"
        , cell (onClick <| InputNumber 5) Single White "5"
        , cell (onClick <| InputNumber 6) Single White "6"
        , cellOp Sub
        , cell (onClick <| InputNumber 1) Single White "1"
        , cell (onClick <| InputNumber 2) Single White "2"
        , cell (onClick <| InputNumber 3) Single White "3"
        , cellOp Add
        , cell (onClick <| InputNumber 0) Single White "0"
        , cell (onClick SetDecimal) Single White "."
        , cell (onClick SetSign) Single White "+/-"
        , cell (onClick Enter) Single Yellow "↵"
        ]


cell : Html.Attribute Msg -> Size -> Color -> String -> Html Msg
cell attr size color content =
    button
        [ class "cell"
        , class <| sizeToString size
        , class <| colorToString color
        , attr
        ]
        [ text content ]


cellOp : Operator -> Html Msg
cellOp op =
    cell (onClick <| InputOperator op) Single Yellow <| operatorToString op


inputBox : Html Msg -> Html Msg
inputBox content =
    div [ class "input-box" ]
        [ content ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
