module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { input : String
    , state : State
    }


type State
    = Init
    | Wait
    | Success User
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Init
    , Cmd.none
    )



-- Update


type Msg
    = Input String
    | Send
    | Receive (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }
            , Cmd.none
            )

        Send ->
            ( { model
                | input = ""
                , state = Wait
              }
            , Http.get
                { url = "https://api.github.com/users/" ++ model.input
                , expect = Http.expectJson Receive userDecoder
                }
            )

        Receive (Ok user) ->
            ( { model | state = Success user }
            , Cmd.none
            )

        Receive (Err error) ->
            ( { model | state = Failed error }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ input
                [ onInput Input
                , value model.input
                , placeholder "github name"
                , autofocus True
                ]
                []
            , button
                [ disabled ((model.state == Wait) || String.isEmpty model.input) ]
                [ text "Submit" ]
            ]
        , case model.state of
            Init ->
                text ""

            Wait ->
                text "waiting..."

            Success user ->
                a
                    [ href user.htmlUrl
                    , target "_blank"
                    ]
                    [ img [ src user.avatarUrl, width 200 ] []
                    , div []
                        [ case user.name of
                            Just name ->
                                text name

                            Nothing ->
                                text ""
                        ]
                    , div [] [ text user.login ]
                    , div []
                        [ case user.bio of
                            Just bio ->
                                text bio

                            Nothing ->
                                text ""
                        ]
                    ]

            Failed error ->
                div [] [ text (Debug.toString error) ]
        ]



-- Data


type alias User =
    { login : String
    , avatarUrl : String
    , name : Maybe String
    , htmlUrl : String
    , bio : Maybe String
    }


userDecoder : Decoder User
userDecoder =
    D.map5
        User
        (D.field "login" D.string)
        (D.field "avatar_url" D.string)
        (D.maybe (D.field "name" D.string))
        (D.field "html_url" D.string)
        (D.maybe (D.field "bio" D.string))
