module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time



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
    { time : Int
    , status : Bool
    , phase : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0, status = False, phase = "Ready" }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Start
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- 60  1 min
        -- 180 2 min (1+2)
        -- 420 4 mins
        --     All
        Tick _ ->
            if model.status then
                if model.time >= 60 && model.time < 180 then
                    ( { model | time = model.time + 1, phase = "Two" }
                    , Cmd.none
                    )

                else if model.time >= 180 && model.time < 420 then
                    ( { model | time = model.time + 1, phase = "Four" }
                    , Cmd.none
                    )

                else if model.time >= 420 then
                    ( { model | time = model.time + 1, phase = "All" }
                    , Cmd.none
                    )

                else
                    ( { model | time = model.time + 1 }
                    , Cmd.none
                    )

            else
                ( { model | time = model.time + 0 }
                , Cmd.none
                )

        Start ->
            ( { model | status = True, phase = "One" }, Cmd.none )

        Stop ->
            ( { model | status = False }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1
            [ style "background-color"
                (if model.status then
                    "Green"

                 else
                    ""
                )
            ]
            [ text "1-2-4-All Timer" ]
        , div [] (statusButtons model.status)
        , h2
            []
            [ phaseText model.phase ]
        ]


phaseText phase =
    case phase of
        "Ready" ->
            div []
                [ hr [] []
                , phaseText "One"
                , hr [] []
                , phaseText "Two"
                , hr [] []
                , phaseText "Four"
                , hr [] []
                , phaseText "All"
                ]

        "One" ->
            div []
                [ p []
                    [ text
                        "1 : Silent self-reflection by individuals on a shared challenge, framed as a question (e.g., What opportunities do YOU see for making progress on this challenge? How would you handle this situation? What ideas or actions do you recommend?)"
                    ]
                , p [] [ text "(1 minute)" ]
                ]

        "Two" ->
            div []
                [ p []
                    [ text
                        "2: Generate ideas in pairs, building on ideas from self-reflection."
                    ]
                , p [] [ text "(2 Minutes)" ]
                ]

        "Four" ->
            div []
                [ p []
                    [ text
                        "4: Share and develop ideas from your pair in foursomes (notice similarities and differences)."
                    ]
                , p [] [ text "(4 minutes)" ]
                ]

        "All" ->
            div []
                [ p []
                    [ text "All: Ask, “What is one idea that stood out in your conversation?” Each group shares one important idea with all."
                    ]
                , p [] [ text "(5 minutes)  repeat as needed" ]
                ]

        _ ->
            div [] [ p [] [ text "This should not ever be seen" ] ]


statusButtons status =
    case status of
        True ->
            [ button [ onClick Stop ] [ text "Stop" ]
            ]

        False ->
            [ button [ onClick Start ] [ text "Start" ]
            ]



-- Building on http://www.liberatingstructures.com/1-1-2-4-all/
-- and https://toolbox.hyperisland.com/1-2-4-all
