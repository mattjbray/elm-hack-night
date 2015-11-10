
import Effects exposing (Never)
import Search
import StartApp
import Task
import Time


app =
  StartApp.start
    { init = Search.init
    , update = Search.update
    , view = Search.view
    , inputs = [Signal.map Search.SetScroll scrollFromBottom]
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port scrollFromBottom : Signal Int
