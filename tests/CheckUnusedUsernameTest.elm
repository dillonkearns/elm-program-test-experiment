module CheckUnusedUsernameTest exposing (..)

import Main
import ProgramTest exposing (ProgramTest)
import Random
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import Test exposing (..)
import Test.Html.Selector exposing (text)
import Test.Runner.Html


start : ProgramTest Main.Model Main.Msg Main.Effect
start =
    ProgramTest.createDocument
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.start ()


all : Test
all =
    describe "check username availability"
        [ test "check username that is available" <|
            \() ->
                start
                    |> ProgramTest.fillIn "username" "Username" "dillonkearns1234"
                    |> ProgramTest.ensureViewHasNot [ text "Available!" ]
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        "/api/username_available/dillonkearns1234"
                        "true"
                    |> ProgramTest.expectViewHas [ text "Available!" ]
        ]


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.GetDeviceList { url, onResult, decoder } ->
            SimulatedEffect.Http.get
                { url = url
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }

        Main.ChangeLight { url, onResult, decoder, body } ->
            SimulatedEffect.Http.post
                { url = url
                , body = SimulatedEffect.Http.jsonBody body
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }

        Main.CheckUsernameAvailable username onResult ->
            SimulatedEffect.Http.get
                { url = "/api/username_available/dillonkearns1234"
                , expect =
                    SimulatedEffect.Http.expectJson onResult (Main.checkUsernameDecoder username)
                }


main =
    let
        config =
            Test.Runner.Html.defaultConfig (Random.initialSeed 0)
    in
    Test.Runner.Html.viewResults config all
