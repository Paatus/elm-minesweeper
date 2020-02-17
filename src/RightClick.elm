module RightClick exposing (onRightClick)

import Html as HTML
import Html.Events as HTMLEvent exposing (custom)
import Json.Decode as Json


onRightClick : msg -> HTML.Attribute msg
onRightClick msg =
    HTMLEvent.custom "contextmenu"
        (Json.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
