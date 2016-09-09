import cats.data.Validated._
import cats.implicits._

val o1 = Option(1)
val o2 = Option(2)

o1 |@| o2

val foo = "foo".validNel[String]
val bar = "bar".validNel[String]

foo |@| bar