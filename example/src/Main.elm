module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import AdjustableTable exposing (..)

-- APP
-- main : Program Never Int Msg
-- main =
--   Html.beginnerProgram { model = model, view = view, update = update }
main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map AdjustableTableMsg <| AdjustableTable.subscriptions model.adjustableTable
    ]

init : (Model, Cmd Msg)
init =
  let
    tableData =
      [ ["1", "2", "3", "4", "5", "6"]
      , ["7", "8", "9", "10", "11", "12"]
      , ["13", "14", "15", "16", "17" , "18"]
      ]
    adjustableTable 
      = AdjustableTable.init
        |> setSettings { resizeColumns = True, reorderColumns = True }
        |> setTableHeadersFromStrings defaultHeaderSettings ["Red", "Blue", "Yellow", "Green", "Pink", "Purple"]
        |> setTableRows (List.map(\d -> getTableRowFromStrings d) tableData)

    model =
      { adjustableTable = adjustableTable
      }
  in
      (model, Cmd.none)

-- MODEL
type alias Model = 
  { adjustableTable : AdjustableTable
  }

model : number
model = 0


-- UPDATE
type Msg = AdjustableTableMsg AdjustableTable.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ adjustableTable } as model) =
    case msg of
        AdjustableTableMsg msg ->
            let
                ( updatedModel, cmd ) =
                    AdjustableTable.update adjustableTable msg
            in
                ( Model updatedModel, Cmd.map AdjustableTableMsg cmd )

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div [ class "table-container" ]
    [ Html.map AdjustableTableMsg <| AdjustableTable.view model.adjustableTable
    ]
