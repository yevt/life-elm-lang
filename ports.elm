-- Ports.elm
port module Ports exposing (..)

-- type alias ScreenSizePortData =
--   { widthPx : String
--   , heigthPx : String
--   }

-- port fileSelected : String -> Cmd msg

-- port fileContentRead : (ImagePortData -> msg) -> Sub msg

port screenSize : ((Int, Int) -> msg) -> Sub msg