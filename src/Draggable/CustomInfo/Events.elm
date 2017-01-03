module Draggable.CustomInfo.Events
    exposing
        ( onDragStart
        , onDragBy
        , onDragEnd
        , onClick
        , onMouseDown
        )

{-| Listeners for the various events involved in dragging (`onDragBy`, `onDragStart`, etc.). Also handles `click` events when the mouse was not moved.
@docs onDragStart, onDragEnd, onDragBy
@docs onClick, onMouseDown
-}

import Internal exposing (Config, Delta)
import Draggable.CustomInfo exposing (Event)


{-| Register a `DragStart` event listener. It will not trigger if the mouse has not moved while it was pressed. It receives the element info.
-}
onDragStart : (info -> msg) -> Event info msg
onDragStart toMsg config =
    { config | onDragStart = Just << toMsg }


{-| Register a `DragEnd` event listener. It will not trigger if the mouse has not moved while it was pressed.
-}
onDragEnd : msg -> Event info msg
onDragEnd toMsg config =
    { config | onDragEnd = Just toMsg }


{-| Register a `DragBy` event listener. It will trigger every time the mouse is moved. The sent message will contain a `Delta`, which is the distance between the current position and the previous one.

    case Msg of
        OnDragBy (dx, dy) ->
            { model | position = { x = position.x + dx, y = position.y + dy } }
-}
onDragBy : (Delta -> msg) -> Event info msg
onDragBy toMsg config =
    { config | onDragBy = Just << toMsg }


{-| Register a `Click` event listener. It will trigger if the mouse is pressed and immediately release, without any move. It receives the element info.
-}
onClick : (info -> msg) -> Event info msg
onClick toMsg config =
    { config | onClick = Just << toMsg }


{-| Register a `MouseDown` event listener. It will trigger whenever the mouse is pressed and will indicate the target element by the given `String` info.
-}
onMouseDown : (info -> msg) -> Event info msg
onMouseDown toMsg config =
    { config | onMouseDown = Just << toMsg }
