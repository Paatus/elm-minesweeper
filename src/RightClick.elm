module RightClick exposing (onRightClick)

import Html as HTML
import Json.Decode as Json
import Svg
import Svg.Events exposing (custom)


onRightClick : msg -> Svg.Attribute msg
onRightClick message =
    custom "contextmenu"
        (Json.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = True
            }
        )
