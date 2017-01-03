module Internal exposing (..)

import Mouse exposing (Position)


type alias Key =
    String


type State info
    = NotDragging
    | DraggingTentative info Position
    | Dragging Position


type Msg info
    = StartDragging info Position
    | DragAt Position
    | StopDragging


type alias Delta =
    ( Float, Float )


type alias Config info msg =
    { onDragStart : info -> Maybe msg
    , onDragBy : Delta -> Maybe msg
    , onDragEnd : Maybe msg
    , onClick : info -> Maybe msg
    , onMouseDown : info -> Maybe msg
    }


type alias Event info msg =
    Config info msg -> Config info msg


defaultConfig : Config info msg
defaultConfig =
    { onDragStart = \_ -> Nothing
    , onDragBy = \_ -> Nothing
    , onDragEnd = Nothing
    , onClick = \_ -> Nothing
    , onMouseDown = \_ -> Nothing
    }


updateAndEmit : Config info msg -> Msg info -> State info -> ( State info, Maybe msg )
updateAndEmit config msg drag =
    case ( drag, msg ) of
        ( NotDragging, StartDragging info initialPosition ) ->
            ( DraggingTentative info initialPosition, config.onMouseDown info )

        ( DraggingTentative info oldPosition, DragAt _ ) ->
            ( Dragging oldPosition
            , config.onDragStart info
            )

        ( Dragging oldPosition, DragAt newPosition ) ->
            ( Dragging newPosition
            , config.onDragBy (distanceTo newPosition oldPosition)
            )

        ( DraggingTentative info _, StopDragging ) ->
            ( NotDragging
            , config.onClick info
            )

        ( Dragging _, StopDragging ) ->
            ( NotDragging
            , config.onDragEnd
            )

        _ ->
            ( drag, Nothing )
                |> logInvalidState drag msg



-- utility


distanceTo : Position -> Position -> Delta
distanceTo end start =
    ( toFloat (end.x - start.x)
    , toFloat (end.y - start.y)
    )


logInvalidState : State info -> Msg info -> a -> a
logInvalidState drag msg result =
    let
        str =
            String.join ""
                [ "Invalid drag state: "
                , toString drag
                , ": "
                , toString msg
                ]

        _ =
            Debug.log str
    in
        result
