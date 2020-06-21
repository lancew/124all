module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
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
    , message : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0, status = False, message = """
    1 : Silent self-reflection by individuals on a shared challenge, framed as a question (e.g., What opportunities do YOU see for making progress on this challenge? How would you handle this situation? What ideas or actions do you recommend?) 1 minute
    2: Generate ideas in pairs, building on ideas from self-reflection. 2 minutes
    4: Share and develop ideas from your pair in foursomes (notice similarities and differences). 4 minutes
    All: Ask, “What is one idea that stood out in your conversation?” Each group shares one important idea with all (repeat cycle as needed). 5 minutes

    """ }, Cmd.none )



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
                if model.time < 60 then
                    ( { model | time = model.time + 1, message = "1 : Silent self-reflection by individuals on a shared challenge, framed as a question (e.g., What opportunities do YOU see for making progress on this challenge? How would you handle this situation? What ideas or actions do you recommend?) 1 minute" }
                    , Cmd.none
                    )

                else if model.time >= 60 && model.time < 180 then
                    ( { model | time = model.time + 1, message = "2: Generate ideas in pairs, building on ideas from self-reflection. 2 minutes" }
                    , Cmd.none
                    )

                else if model.time >= 180 && model.time < 420 then
                    ( { model | time = model.time + 1, message = "4: Share and develop ideas from your pair in foursomes (notice similarities and differences). 4 minutes" }
                    , Cmd.none
                    )

                else if model.time >= 420 then
                    ( { model | time = model.time + 1, message = "All: Ask, “What is one idea that stood out in your conversation?” Each group shares one important idea with all (repeat cycle as needed). 5 minutes" }
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
            ( { model | status = True }, Cmd.none )

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
        [ h1 []
            [ text "1-2-4-All Timer" ]
        , button [ onClick Start ] [ text "Start" ]
        , button [ onClick Stop ] [ text "Stop" ]
        , h2
            []
            [ text (String.fromInt model.time) ]
        , h3
            []
            [ text model.message ]
        ]



-- Building on http://www.liberatingstructures.com/1-1-2-4-all/
-- and https://toolbox.hyperisland.com/1-2-4-all
