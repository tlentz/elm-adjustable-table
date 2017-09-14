# elm-adjustable-table

```shell
elm package install tlentz/elm-adjustable-table
```

## Usage

The `AdjustableTable.init` function creates a default Adjustable Table which an empty list of headers, an empty list of rows, and both resizeColumns and reorderColumns set to True.
```elm
    tableData =
        [ ["Red 1", "Blue 1", "Yellow 1"]
        , ["Red 2", "Blue 2", "Yellow 2"]
        , ["Red 3", "Blue 3", "Yellow 3"]
        ]
    myTable =
        AdjustableTable.init
            |> setSettings { resizeColumns = True, reorderColumns = False }
            |> setTableHeadersFromStrings defaultHeaderSettings ["Red", "Blue", "Yellow"]
            |> setTableRows (List.map (\d -> getTableRowFromStrings d) tableData)
```

Because the Adjustable Table uses mouse movements for resizing and reording columns, it requires subscriptions.
After initialization, handle the subscriptions.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map AdjustableTableMsg <| AdjustableTable.subscriptions model.adjustableTable
    ]
```

Handle the updates from the subscription in your update function
```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ adjustableTable } as model) =
    case msg of
        AdjustableTableMsg msg ->
            let
                ( updatedModel, cmd ) =
                    AdjustableTable.update adjustableTable msg
            in
                ( Model updatedModel, Cmd.map AdjustableTableMsg cmd )
```

To view the table, just call the view function
```elm
Html.map AdjustableTableMsg <| AdjustableTable.view model.adjustableTable
```

## Example

Checkout the [example](https://github.com/tlentz/elm-adjustable-table/tree/master/example "elm-adjustable-table example") to test it or see how to wire it up.

![See it in action!](example.gif)