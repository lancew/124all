module Main exposing (main)

import Browser
import Color.White
import Element exposing (alignLeft, alignRight, centerX, column, el, fill, fillPortion, height, layout, link, padding, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Time
import TypedTime
import Update.Extra as Update



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
    { timer : Int
    , status : Bool
    , phase : Phase
    , next : Phase
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { timer = 60, status = False, next = One, phase = Ready }, Cmd.none )



-- UPDATE


type Phase
    = One
    | Two
    | Four
    | All
    | Ready


type Msg
    = Tick Time.Posix
    | Start
    | Stop
    | ChangePhase Phase
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | status = True }, Cmd.none ) |> Update.andThen update (changePhase One)

        Stop ->
            ( { model | status = False }, Cmd.none )

        Restart ->
            ( { model | status = True }, Cmd.none )

        ChangePhase stage ->
            case stage of
                One ->
                    ( { model | phase = One, next = Two, timer = 60 }, Cmd.none )

                Two ->
                    ( { model | phase = Two, next = Four, timer = 120 }, Cmd.none )

                Four ->
                    ( { model | phase = Four, next = All, timer = 240 }, Cmd.none )

                All ->
                    ( { model | phase = All, next = All, timer = 300 }, Cmd.none )

                Ready -> (model, Cmd.none)    

        Tick _ ->
            if model.status then
                if model.timer > 0 then
                    ( { model | timer = model.timer - 1 }, Cmd.none )

                else
                    case model.next of
                        Two ->
                            ( { model | timer = 1 }, Cmd.none ) |> Update.andThen update (changePhase Two)

                        Four ->
                            ( { model | timer = 1 }, Cmd.none ) |> Update.andThen update (changePhase Four)

                        All ->
                            ( { model | timer = 1 }, Cmd.none ) |> Update.andThen update (changePhase All)

                        _ ->
                            ( model, Cmd.none )

            else
                ( { model | timer = model.timer - 0 }
                , Cmd.none
                )


changePhase stage =
    case stage of
        One ->
            ChangePhase One

        Two ->
            ChangePhase Two

        Four ->
            ChangePhase Four

        All ->
            ChangePhase All

        Ready -> ChangePhase Ready    



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html.Html Msg
view model =
    Element.layout [ width fill, height fill, padding 10, Background.gradient { angle = 0, steps = [ Color.White.seashell, Color.White.whitesmoke ] } ] <|
        column
            [ width fill ]
            [ header
            , row [ centerX, Font.size 32, Border.width 2, Border.rounded 6, padding 10 ]
                [ el [] (text (TypedTime.toString TypedTime.Seconds (TypedTime.seconds (toFloat model.timer))))
                ]
            , row [ padding 5 ] []
            , row
                [ Border.width 2
                , Border.rounded 6
                , padding 10
                , centerX
                ]
                [ statusButtons model ]
            , row [] [ phaseText model.phase ]
            , footer
            ]


header : Element.Element Msg
header =
    row [ centerX, padding 20 ]
        [ el [ Font.size 48, Font.center, Font.bold ] <| text "1-2-4-All Timer"
        ]


phaseText : Phase -> Element.Element Msg
phaseText phase =
    case phase of
        Ready ->
            column [ width fill ]
                [ row [ width fill ] [ phaseText One ]
                , row [ width fill ] [ phaseText Two ]
                , row [ width fill ] [ phaseText Four ]
                , row [ width fill ] [ phaseText All ]
                ]

        One ->
            row [ width fill ]
                [ column [ width (fillPortion 5) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "1 : Silent self-reflection by individuals on a shared challenge, framed as a question (e.g., What opportunities do YOU see for making progress on this challenge? How would you handle this situation? What ideas or actions do you recommend?)" ] ]
                , column [ width (fillPortion 2) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "(1 minute)" ] ]
                ]

        Two ->
            row [ width fill ]
                [ column [ width (fillPortion 5) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "2: Generate ideas in pairs, building on ideas from self-reflection." ] ]
                , column [ width (fillPortion 2) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "(2 Minutes)" ] ]
                ]

        Four ->
            row [ width fill ]
                [ column [ width (fillPortion 5) ]
                    [ paragraph [ width fill, padding 20, padding 20 ]
                        [ text
                            "4: Share and develop ideas from your pair in foursomes (notice similarities and differences)."
                        ]
                    ]
                , column [ width (fillPortion 2) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "(4 minutes)" ] ]
                ]

        All ->
            row [ width fill ]
                [ column [ width (fillPortion 5) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "All: Ask, “What is one idea that stood out in your conversation?” Each group shares one important idea with all." ] ]
                , column [ width (fillPortion 2) ] [ paragraph [ width fill, padding 20, padding 20 ] [ text "(5 minutes)  repeat as needed" ] ]
                ]


statusButtons : Model -> Element.Element Msg
statusButtons model =
    if model.status then
        Input.button [] { onPress = Just Stop, label = text "Stop" }

    else if model.phase == Ready then
        Input.button [] { onPress = Just Start, label = text "Start" }

    else
        Input.button [] { onPress = Just Restart, label = text "Start" }


footer : Element.Element Msg
footer =
    row [ padding 20, spacing 20, width fill, Border.width 1, Border.dotted, Border.rounded 6 ]
        [ column [ width (fillPortion 3) ] [ link [ alignLeft ] { url = "https://lancew.github.io/124all/", label = text "https://lancew.github.io/124all/" } ]
        , column [ width (fillPortion 1) ]
            [ link [ alignRight ] { url = "https://github.com/lancew/124all", label = text "By Lance Wicks: https://github.com/lancew/124all" }
            , link [ alignRight ] { url = "http://www.liberatingstructures.com/1-1-2-4-all/", label = text "Based on ideas in Liberating Structures" }
            , link [ alignRight ] { url = "https://toolbox.hyperisland.com/1-2-4-all", label = text "and Hyper Island" }
            ]
        ]



-- Building on http://www.liberatingstructures.com/1-1-2-4-all/
-- and https://toolbox.hyperisland.com/1-2-4-all
