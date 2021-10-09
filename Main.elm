module Main exposing (..)


  
import Browser
import Html exposing (Html, text)
import Task
import Time
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, maybe, int, string)
import Http exposing (Error)
import GraphQl exposing (Operation, Variables, Query, Named)
import GraphQl.Http



type Msg
  = Start | GraphQlMsg (Result Error NameAndAddress)

type alias User =
  { id : Maybe Int
  , name : Maybe Name
  }

type alias Name =
  { firstName : Maybe String
  , lastName : Maybe String
  }

type alias Address =
  { street : Maybe String
  , town : Maybe String
  }

type alias NameAndAddress =
  { user : User
  , address : Address
  }

decodeName : Decoder Name
decodeName =
  Decode.map2 Name
    (maybe (field "first_name" string))
    (maybe (field "last_name" string))

decodeUser : Decoder User
decodeUser =
  Decode.map2 User
    (maybe (field "id" int))
    (maybe (field "name" decodeName))

decodeAddress : Decoder Address
decodeAddress =
  Decode.map2 Address
    (maybe (field "street" string))
    (maybe (field "town" string))

decodeNameAndAddress : Decoder NameAndAddress
decodeNameAndAddress =
  Decode.map2 NameAndAddress
    (field "user" decodeUser)
    (field "address" decodeAddress)


userRequest : Operation Query Variables
userRequest =
  GraphQl.named "MySuperQuery"
    [ GraphQl.field "user"
      |> GraphQl.withArgument "id" (GraphQl.variable "id")
      |> GraphQl.withAlias "current_user"
      |> GraphQl.withSelectors
        [ GraphQl.field "id"
        , GraphQl.field "name"
          |> GraphQl.withSelectors
            [ GraphQl.field "first_name"
            , GraphQl.field "last_name"
            ]
        ]
    , GraphQl.field "address"
      |> GraphQl.withArgument "city" (GraphQl.string "Paris")
      |> GraphQl.withArgument "id" (GraphQl.int 12)
      |> GraphQl.withArgument "type" (GraphQl.type_ "LOFT")
      |> GraphQl.withSelectors
        [ GraphQl.field "street"
        , GraphQl.field "town"
        ]
    ]
    |> GraphQl.withVariables [ ("id", "123") ]

graphQlRequestOptions : GraphQl.Http.Options
graphQlRequestOptions =
  { url = "https://example.com"
  , headers = []
  }

sendRequest : Int -> Cmd Msg
sendRequest id =
  GraphQl.query userRequest
  |> GraphQl.addVariables [ ("id", Encode.int id) ]
  |> GraphQl.Http.send graphQlRequestOptions GraphQlMsg decodeNameAndAddress
  
  
  
  
  
  
  
  
  
 
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
  { v : Int
  }


init : () -> (Model, Cmd Msg)
init _ = ( Model 1, sendRequest 1)



-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = ( model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
view : Model -> Html Msg
view model =  text "Hello"
