-- Todo
-- 1. Convert Month/Year inputs to dropdowns
-- 2. Pretty up content with CSS

module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Helpers exposing (isValidCreditCardNumber)
import Constants exposing (..)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { creditCardNumber : String
  , ccv : String
  , expirationDate : CCExpiryDate
  }

type alias CCExpiryDate = 
  { month : String
  , year : String
  }

init : Model
init =
  Model "" "" {month = "", year = ""}

-- UPDATE

type Msg
  = CreditCardNumber String
  | CCV String
  | ExpirationMonth String
  | ExpirationYear String

update : Msg -> Model -> Model
update msg model =
  case msg of
    CreditCardNumber newCreditCardNumber ->
      { model | creditCardNumber = newCreditCardNumber }

    CCV newCCV ->
      { model | ccv = newCCV }

    ExpirationMonth newExpirationMonth ->
      { model | expirationDate = {year = model.expirationDate.year, month = newExpirationMonth}}
    
    ExpirationYear newExpirationYear ->
      { model | expirationDate = {year = newExpirationYear, month = model.expirationDate.month}}

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Credit Card Number" model.creditCardNumber CreditCardNumber (Just ccLength)
    , viewInput "text" "CCV" model.ccv CCV (Just ccvLength)
    , viewInput "text" "Expiration Month" model.expirationDate.month ExpirationMonth Nothing
    , viewInput "text" "Expiration Year" model.expirationDate.year ExpirationYear Nothing
    , viewValidation model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Maybe Int -> Html msg
viewInput t p v toMsg maxLength = case maxLength of
  Just ml -> input [ type_ t, placeholder p, value v, onInput toMsg, maxlength ml ] []
  Nothing -> input [ type_ t, placeholder p, value v, onInput toMsg ] []

-- VALIDATION

isFormValid : Model -> Bool
isFormValid model = 
  isValidCreditCardNumber(model.creditCardNumber)

viewValidation : Model -> Html msg
viewValidation model =
  if isFormValid(model) then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Validation Error!" ]