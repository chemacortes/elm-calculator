module CalculatorTest exposing (..)

import Main
import ProgramTest exposing (clickButton, expectViewHas, start)
import Test exposing (Test, describe, test)
import Test.Html.Selector exposing (text)


all : Test
all =
    describe "basic arithmetic"
        [ test "20 x 3 = 60" <|
            \() ->
                ProgramTest.createDocument
                    { init = \_ -> Main.init
                    , view = Main.view
                    , update = Main.update
                    }
                    |> start ()
                    |> clickButton "CE"
                    |> clickButton "2"
                    |> clickButton "0"
                    |> clickButton "↵"
                    |> clickButton "3"
                    |> clickButton "×"
                    |> expectViewHas [ text "60" ]
        ]
