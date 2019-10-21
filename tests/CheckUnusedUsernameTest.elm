module CheckUnusedUsernameTest exposing (..)

import Browser
import Dict exposing (Dict)
import Expect
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode
import ProgramTest exposing (ProgramTest)
import Random
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import Test exposing (..)
import Test.Html.Selector exposing (class, text)
import Test.Runner.Html


apiBase =
    "http://localhost:8003/lighting_service/v1"


type WebData a
    = Loading
    | Loaded a
    | Error Http.Error


mapWebData : (a -> b) -> WebData a -> WebData b
mapWebData f data =
    case data of
        Loading ->
            Loading

        Loaded a ->
            Loaded (f a)

        Error x ->
            Error x


type alias Model =
    { lights : WebData (List Light)
    , pending : Dict String PostResult
    }


type PostResult
    = Waiting
    | Failed Http.Error


initialModel : Model
initialModel =
    { lights = Loading
    , pending = Dict.empty
    }


type Msg
    = OnUsernameInput String


type alias Light =
    { id : String
    , name : String
    , state : LightState
    }


type LightState
    = OnOff Bool
    | Dimmable Float


type alias Flags =
    ()


main_ : Program Flags Model Msg
main_ =
    Browser.document
        { init =
            \flags ->
                init flags
                    |> Tuple.mapSecond perform
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond perform
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Effect )
init () =
    ( initialModel, NoEffect )


subscriptions _ =
    Sub.none


type Effect
    = NoEffect
    | GetDeviceList
        { url : String
        , decoder : Json.Decode.Decoder (List Light)
        , onResult : Result Http.Error (List Light) -> Msg
        }
    | ChangeLight
        { url : String
        , body : Json.Encode.Value
        , decoder : Json.Decode.Decoder Light
        , onResult : Result Http.Error Light -> Msg
        }


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        GetDeviceList { url, onResult, decoder } ->
            Http.get
                { url = url
                , expect = Http.expectJson onResult decoder
                }

        ChangeLight { url, onResult, decoder, body } ->
            Http.post
                { url = url
                , body = Http.jsonBody body
                , expect = Http.expectJson onResult decoder
                }


lightDecoder : Json.Decode.Decoder Light
lightDecoder =
    let
        stateDecoder =
            Json.Decode.map2 toState
                (Json.Decode.field "dimmable" Json.Decode.bool)
                (Json.Decode.field "value" Json.Decode.float)

        toState isDimmable value =
            case isDimmable of
                True ->
                    Dimmable value

                False ->
                    if value <= 0.0 then
                        OnOff False

                    else
                        OnOff True
    in
    Json.Decode.map3 Light
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        stateDecoder


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        OnUsernameInput newInput ->
            ( model
            , NoEffect
            )


view model =
    { title = "Lighting control"
    , body =
        [ usernameInput
        , Html.text "Available!"
        ]
    }


usernameInput =
    Html.div []
        [ Html.label
            [ Html.Attributes.for "username"
            ]
            [ Html.text "Username"
            ]
        , Html.textarea [ Html.Attributes.id "username" ] []
        ]


start : ProgramTest Model Msg Effect
start =
    ProgramTest.createDocument
        { init = init
        , update = update
        , view = view
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
                    |> ProgramTest.expectViewHas [ text "Available!" ]

        --                    |> ProgramTest.simulateHttpOk
        --                        "GET"
        --                        "http://localhost:8003/lighting_service/v1/devices"
        --                        """[{"id":"K001", "name":"Kitchen", "dimmable":false, "value":0}]"""
        --                    |> ProgramTest.clickButton "Turn on"
        --                    |> ProgramTest.expectHttpRequest
        --                        "POST"
        --                        "http://localhost:8003/lighting_service/v1/devices/K001"
        --                        (.body >> Expect.equal """{"value":1}""")
        ]


simulateEffects : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects effect =
    case effect of
        NoEffect ->
            SimulatedEffect.Cmd.none

        GetDeviceList { url, onResult, decoder } ->
            SimulatedEffect.Http.get
                { url = url
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }

        ChangeLight { url, onResult, decoder, body } ->
            SimulatedEffect.Http.post
                { url = url
                , body = SimulatedEffect.Http.jsonBody body
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }


main =
    let
        config =
            Test.Runner.Html.defaultConfig (Random.initialSeed 0)
    in
    Test.Runner.Html.viewResults config all
