module Hello exposing (..)

import Browser
import Html exposing (Html, button, div, h3, img, input, node, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, disabled, id, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import List exposing (product)


initialModel : Model
initialModel =
    [ { name = "Cheese", photo = "cheese.jpg", quantity = 1, unitPrice = 6 }
    , { name = "Bread", photo = "cheese.jpg", quantity = 2, unitPrice = 3 }
    ]


inventoryModel : Model
inventoryModel =
    [ { name = "Cheese", photo = "cheese.jpg", quantity = 1, unitPrice = 6 }
    , { name = "Bread", photo = "cheese.jpg", quantity = 1, unitPrice = 3 }
    , { name = "Oil", photo = "cheese.jpg", quantity = 1, unitPrice = 9 }
    ]


type alias Product =
    { name : String, photo : String, quantity : Int, unitPrice : Int }


type alias Model =
    List Product


type Msg
    = Quantities Int
    | AddToCart String
    | ProductQuantity String String


cartView : Model -> Html Msg
cartView model =
    div [ id "first-app" ]
        [ node "link"
            [ attribute "rel" "stylesheet"
            , attribute "href" "main.css"
            ]
            []
        , node "link"
            [ attribute "rel" "stylesheet"
            , attribute "href" "https://cdn.jsdelivr.net/npm/bootstrap@5.2.2/dist/css/bootstrap.min.css"
            ]
            []
        , h3 [ style "font-size" "200%", class "title" ]
            [ List.map (\p -> p.quantity) model
                |> List.sum
                |> String.fromInt
                |> text
            ]
        , table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Product" ]
                    , th [] [ text "Photo" ]
                    , th [] [ text "Quantity" ]
                    , th [] [ text "Price" ]
                    ]
                ]
            , tbody []
                (List.map itemView model)
            ]
        , div []
            [ text "Total amount: $"
            , List.map (\p -> p.quantity * p.unitPrice) model
                |> List.sum
                |> String.fromInt
                |> text
            ]
        , button
            [ class "btn btn-primary"
            , onClick (Quantities 2)
            ]
            [ text "Increment Quantities" ]
        , button
            [ class "btn btn-secondary"
            , onClick (Quantities 1)
            ]
            [ text "Decrement Quantities" ]
        , button
            [ class "btn btn-danger"
            , onClick (Quantities 0)
            ]
            [ text "Reset Quantities" ]
        , button
            [ class "btn btn-outline-primary"
            , disabled (List.length model == 0)
            , onClick (Quantities 0)
            ]
            [ text "Place order" ]
        , div [ class "container" ]
            [ div [ class "row text-centered" ]
                (List.map inventoryItemView inventoryModel)
            ]
        ]


update : Msg -> Model -> Model
update message model =
    let
        changeQuantity q =
            List.map (\product -> { product | quantity = product.quantity + q }) model
                |> List.filter (\p -> p.quantity > 0)
    in
    case message of
        Quantities 2 ->
            changeQuantity 1

        Quantities 1 ->
            changeQuantity -1

        Quantities _ ->
            initialModel

        AddToCart n ->
            if List.any (\p -> p.name == n) model then
                List.map
                    (\p ->
                        if p.name == n then
                            { p
                                | quantity = p.quantity + 1
                            }

                        else
                            p
                    )
                    model

            else
                List.append (List.filter (\p -> p.name == n) inventoryModel) model

        ProductQuantity targetName inputQuantity ->
            List.map
                (\p ->
                    if p.name == targetName then
                        { p
                            | quantity =
                                String.toInt inputQuantity
                                    |> Maybe.withDefault 1
                        }

                    else
                        p
                )
                model
                |> List.filter (\p -> p.quantity > 0)


type alias Item a =
    { a | quantity : Int, unitPrice : Int }


calculatePrice : Item a -> String
calculatePrice extensible =
    extensible.quantity * extensible.unitPrice |> String.fromInt


itemView : Product -> Html Msg
itemView product =
    tr []
        [ td [] [ text product.name ]
        , td [] [ img [ src product.photo, width 100 ] [] ]
        , td []
            [ input
                [ type_ "number"
                , value (String.fromInt product.quantity)
                , onInput (ProductQuantity product.name)
                ]
                []
            ]
        , td [] [ text <| calculatePrice product ]
        ]


inventoryItemView : Product -> Html Msg
inventoryItemView product =
    div [ class "col" ]
        [ img [ src product.photo, width 100 ] []
        , div [] [ text product.name ]
        , div [] [ text ("$ " ++ String.fromInt product.unitPrice) ]
        , div []
            [ button
                [ class "btn btn-outline-primary"
                , onClick (AddToCart product.name)
                ]
                [ text "Add to cart" ]
            ]
        ]


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = cartView
        }
