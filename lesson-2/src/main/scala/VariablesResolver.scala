package aazazello.expressioncalculator

import scala.collection.mutable.HashMap
import scala.util.Try


object VariablesResolver {
    private var values = Map[String, AnyVal]() 

    def init(vars: String*): Unit = {
        vars.foreach(v => {
            val vs = v.split(":")
            vs.length match {
                case 0 | 1 => { println(s"Not splitted ${v}") }
                case 2 => {
                    vs.last match {
                        case s if Try { s.toDouble }.toOption != None => { values += (vs.head -> s.toDouble) }
                        case b if Try { b.toBoolean }.toOption != None => { values += (vs.head -> b.toBoolean) }
                        case _ => {  println(s"Not parsed ${v.head} -> ${v.last}") }
                    
                    }
                }
            }
        })

        values.keys.foreach(k => println(s"${k} = ${get(k)}"))
    }

    def get(variable: String): Option[AnyVal] = {
        variable.toLowerCase match {
            case "e" => Some(math.E)
            case "pi" => Some(math.Pi)
            case _ => values get variable
        }
    }
}
