module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (autofocus, class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Random exposing (Seed, initialSeed)
import Task
import Time exposing (Posix, now, posixToMillis)



---- MODEL ----


type alias Model =
    { whiteDieOne : Int
    , whiteDieTwo : Int
    , blueDie : Int
    , redDie : Int
    , greenDie : Int
    , yellowDie : Int
    , nextSeed : Seed
    }


init : ( Model, Cmd Msg )
init =
    ( { whiteDieOne = 1
      , whiteDieTwo = 1
      , redDie = 1
      , greenDie = 1
      , blueDie = 1
      , yellowDie = 1
      , nextSeed = initialSeed 0
      }
    , createInitialTimeCommand
    )


createInitialTimeCommand : Cmd Msg
createInitialTimeCommand =
    now |> Task.perform InitialTimeUpdated



---- UPDATE ----


type Msg
    = RollDice
    | InitialTimeUpdated Posix


mapRandomsToDice : Model -> List Int -> Model
mapRandomsToDice model newNumbers =
    case newNumbers of
        [ whiteOne, whiteTwo, red, yellow, green, blue ] ->
            { model
                | whiteDieOne = whiteOne
                , whiteDieTwo = whiteTwo
                , redDie = red
                , yellowDie = yellow
                , greenDie = green
                , blueDie = blue
            }

        _ ->
            model


createRandoms : Seed -> ( List Int, Seed )
createRandoms seed =
    let
        generator =
            Random.list 6 (Random.int 1 6)
    in
    Random.step generator seed


rollAndUpdateModel : Model -> Seed -> Model
rollAndUpdateModel model seed =
    let
        ( newNumbers, nextSeed ) =
            createRandoms seed

        modelWithNewNumbers =
            mapRandomsToDice model newNumbers
    in
    { modelWithNewNumbers | nextSeed = nextSeed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( rollAndUpdateModel model model.nextSeed, Cmd.none )

        InitialTimeUpdated time ->
            ( time |> posixToMillis |> initialSeed |> rollAndUpdateModel model, Cmd.none )



---- VIEW ----


getDiceSumAsText : Int -> Int -> Html never
getDiceSumAsText first second =
    first + second |> String.fromInt |> text


getDieValueAsText : Int -> Html never
getDieValueAsText die =
    die |> String.fromInt |> text


multipleClasses : List String -> Html.Attribute never
multipleClasses classes =
    let
        toTuple c =
            ( c, True )
    in
    List.map toTuple classes
        |> classList


view : Model -> Html Msg
view model =
    let
        blockClasses =
            [ "block" ]

        generateDiv classes textFn =
            div [ multipleClasses (List.concat [ blockClasses, classes ]) ] [ textFn ]

        whiteClasses =
            [ "white" ]

        whiteDieClasses =
            "white-border" :: whiteClasses

        redClasses =
            [ "red" ]

        redDieClasses =
            "red-border" :: redClasses

        yellowClasses =
            [ "yellow" ]

        yellowDieClasses =
            "yellow-border" :: yellowClasses

        greenClasses =
            [ "green" ]

        greenDieClasses =
            "green-border" :: greenClasses

        blueClasses =
            [ "blue" ]

        blueDieClasses =
            "blue-border" :: blueClasses

        blockList =
            [ ( whiteClasses, getDiceSumAsText model.whiteDieOne model.whiteDieTwo )
            , ( whiteDieClasses, getDieValueAsText model.whiteDieOne )
            , ( whiteDieClasses, getDieValueAsText model.whiteDieTwo )
            , ( redDieClasses, getDieValueAsText model.redDie )
            , ( redClasses, getDiceSumAsText model.whiteDieOne model.redDie )
            , ( redClasses, getDiceSumAsText model.whiteDieTwo model.redDie )
            , ( yellowDieClasses, getDieValueAsText model.yellowDie )
            , ( yellowClasses, getDiceSumAsText model.whiteDieOne model.yellowDie )
            , ( yellowClasses, getDiceSumAsText model.whiteDieTwo model.yellowDie )
            , ( greenDieClasses, getDieValueAsText model.greenDie )
            , ( greenClasses, getDiceSumAsText model.whiteDieOne model.greenDie )
            , ( greenClasses, getDiceSumAsText model.whiteDieTwo model.greenDie )
            , ( blueDieClasses, getDieValueAsText model.blueDie )
            , ( blueClasses, getDiceSumAsText model.whiteDieOne model.blueDie )
            , ( blueClasses, getDiceSumAsText model.whiteDieTwo model.blueDie )
            ]

        blockElements =
            List.map (\blockDef -> generateDiv (Tuple.first blockDef) (Tuple.second blockDef)) blockList
    in
    div []
        [ div [ class "grid" ]
            blockElements
        , button [ class "roll-button", autofocus True, type_ "button", onClick RollDice ] [ text "Roll again" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }