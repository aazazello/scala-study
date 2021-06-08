import scala.io.StdIn
import aazazello.expressioncalculator._

object Lesson_2 {

    @main def calculate(expression: String, vars: String*): Unit = {
  
        VariablesResolver.init(vars*)
        
        //println(expr.parse(expression).getClass().getCanonicalName())
        var expr = new Expression().parse(expression)
        var (x, ex) = expr match {
                case Right(x) => { (s"Parsed :${x.source}: successfully", x) }
                case Left(x) =>  { (s"An error $x occured", null) }
                }

        println(x)
        if (ex != null) {
            val result = new Calculator(VariablesResolver).calculate(ex)
            println(s"calculated ${result.value}")
        }

        StdIn.readLine()
    }

}