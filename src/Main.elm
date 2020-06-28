module Main exposing (main)

import Browser
import Element exposing (alignRight, centerX, column, el, link,fill, fillPortion, height, layout, padding, paragraph, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Time



-- MAIN


main : Program () Model Msg
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
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view model =
    Element.layout [ width fill, height fill, padding 10, Background.gradient { angle = 0, steps = [ rgb255 167 180 193, rgb255 255 255 255 ] } ] <|
        column
            [ width fill ]
            [ header
            , row
                [ Border.width 2
                , Border.rounded 6
                , padding 5
                ]
                [ statusButtons model.status ]
            , row [] [ phaseText model.phase ]
            , footer
            ]


header =
    row [ centerX, padding 20 ]
        [ el [ Font.size 48, Font.center, Font.bold ] <| text "1-2-4-All Timer"
        ]


phaseText phase =
    case phase of
        "Ready" ->
            column [ width fill ]
                [ row [ width fill ] [ phaseText "One" ]
                , row [ width fill ] [ phaseText "Two" ]
                , row [ width fill ] [ phaseText "Four" ]
                , row [ width fill ] [ phaseText "All" ]
                ]

        "One" ->
            row [ width fill ]
                [ column [ width (fillPortion 5) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "1 : Silent self-reflection by individuals on a shared challenge, framed as a question (e.g., What opportunities do YOU see for making progress on this challenge? How would you handle this situation? What ideas or actions do you recommend?)" ] ]
                , column [ width (fillPortion 2) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "(1 minute)" ] ]
                ]

        "Two" ->
            row [ width fill ]
                [ column [ width (fillPortion 5) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "2: Generate ideas in pairs, building on ideas from self-reflection." ] ]
                , column [ width (fillPortion 2) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "(2 Minutes)" ] ]
                ]

        "Four" ->
            row [ width fill ]
                [ column [ width (fillPortion 5) ]
                    [ paragraph [ width fill, padding 20, padding 20 ]
                        [ text
                            "4: Share and develop ideas from your pair in foursomes (notice similarities and differences)."
                        ]
                    ]
                , column [ width (fillPortion 2) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "(4 minutes)" ] ]
                ]

        "All" ->
            row [ width fill ]
                [ column [ width (fillPortion 5) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "All: Ask, “What is one idea that stood out in your conversation?” Each group shares one important idea with all." ] ]
                , column [ width (fillPortion 2) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "(5 minutes)  repeat as needed" ] ]
                ]

        _ ->
            row [] [ el [] (text "Something went wrong") ]


statusButtons status =
    if status then
        Input.button [] { onPress = Just Stop, label = text "Stop" }

    else
        Input.button [] { onPress = Just Start, label = text "Start" }


footer =
    row [ width fill, padding 20, spacing 20 ]
        [ link [ alignRight ]  { url = "https://github.com/lancew/124all",label = text "By Lance Wicks: https://github.com/lancew/124all"}
        ]



-- Building on http://www.liberatingstructures.com/1-1-2-4-all/
-- and https://toolbox.hyperisland.com/1-2-4-all
