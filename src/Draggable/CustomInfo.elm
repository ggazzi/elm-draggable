module Draggable.CustomInfo
    exposing
        ( State
        , Msg
        , Delta
        , Config
        , Event
        , basicConfig
        , customConfig
        , mouseTrigger
        , init
        , update
        , subscriptions
        )

{-|
This library provides and easy way to make DOM elements (Html or Svg) draggable, extracting custom information from the mouse events.

## When is dragging considered?
An element is considered to be dragging when the mouse is pressed **and** moved before it is released. Otherwise, the action is considered a click. This is useful because in some cases you may want to support both actions.

[See examples](https://github.com/zaboco/elm-draggable/tree/master/examples)


# Initial State
@docs init

# Config
@docs basicConfig, customConfig

# Update
@docs update, subscriptions

# DOM trigger
@docs mouseTrigger

# Definitions
@docs Delta, State, Msg, Config, Event
-}

import Cmd.Extra
import Internal
import Json.Decode as Json
import Mouse exposing (Position)
import VirtualDom


{-| A type alias representing the distance between two drag points.
-}
type alias Delta =
    ( Float, Float )


{-| Drag state to be included in model.
-}
type State info
    = State (Internal.State info)


{-| A message type for updating the internal drag state.
-}
type Msg info
    = Msg (Internal.Msg info)


{-| An event declaration for the draggable config.
-}
type alias Event info msg =
    Internal.Event info msg


{-| Initial drag state
-}
init : State info
init =
    State Internal.NotDragging


{-| Handle update messages for the draggable model. It assumes that the drag state will be stored under the key `drag`.
-}
update :
    Config info msg
    -> Msg info
    -> { m | drag : State info }
    -> ( { m | drag : State info }, Cmd msg )
update config msg model =
    let
        ( dragState, dragCmd ) =
            updateDraggable config msg model.drag
    in
        { model | drag = dragState } ! [ dragCmd ]


updateDraggable : Config info msg -> Msg info -> State info -> ( State info, Cmd msg )
updateDraggable (Config config) (Msg msg) (State drag) =
    let
        ( newDrag, newMsgMaybe ) =
            Internal.updateAndEmit config msg drag
    in
        ( State newDrag, Cmd.Extra.optionalMessage newMsgMaybe )


{-| Handle mouse subscriptions used for dragging
-}
subscriptions : (Msg info -> msg) -> State info -> Sub msg
subscriptions envelope (State drag) =
    case drag of
        Internal.NotDragging ->
            Sub.none

        _ ->
            [ Mouse.moves Internal.DragAt, Mouse.ups (\_ -> Internal.StopDragging) ]
                |> Sub.batch
                |> Sub.map (envelope << Msg)


{-| DOM event handler to start dragging on mouse down. It requires a JSON parser, which may extract information from the click event.

A common use case is providing a `String` key for the element, in order to provide support for multiple drag targets sharing the same drag state.

    div [ mouseTrigger DragMsg (Json.Decode.succeed "element-id") ] [ text "Drag me" ]
-}
mouseTrigger : (Msg info -> msg) -> Json.Decoder info -> VirtualDom.Property msg
mouseTrigger envelope infoDecoder =
    let
        ignoreDefaults =
            VirtualDom.Options True True

        makeMsg info position =
            envelope <| Msg <| Internal.StartDragging info position
    in
        VirtualDom.onWithOptions "mousedown"
            ignoreDefaults
            (Json.map2 makeMsg infoDecoder Mouse.position)



-- CONFIG


{-| Configuration of a draggable model.
-}
type Config info msg
    = Config (Internal.Config info msg)


{-| Basic config

    config = basicConfig OnDragBy
-}
basicConfig : (Delta -> msg) -> Config info msg
basicConfig onDragByListener =
    let
        defaultConfig =
            Internal.defaultConfig
    in
        Config { defaultConfig | onDragBy = Just << onDragByListener }


{-| Custom config, including arbitrary options. See [`Events`](#Draggable-CustomInfo-Events).

    config = customConfig
        [ onDragBy OnDragBy
        , onDragStart OnDragStart
        , onDragEnd OnDragEnd
        ]
-}
customConfig : List (Event info msg) -> Config info msg
customConfig events =
    Config <| List.foldl (<|) Internal.defaultConfig events
