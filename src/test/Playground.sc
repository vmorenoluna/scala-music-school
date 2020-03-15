import music.Types.{Step, hs, ws}

val pattern: Stream[Step] = List(ws, ws, hs, ws, ws, ws, hs).toStream #::: pattern

println(pattern.drop(1).take(7).toList)

val list = List(1,2,3,4,5)

println(list.view.take(3))