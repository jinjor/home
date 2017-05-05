module Core exposing (..)


andThen : (model1 -> ( model2, Cmd msg )) -> ( model1, Cmd msg ) -> ( model2, Cmd msg )
andThen f ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            f model
    in
        newModel ! [ cmd, newCmd ]
