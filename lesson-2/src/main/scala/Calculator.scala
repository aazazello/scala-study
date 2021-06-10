package aazazello.expressioncalculator

import scala.collection.mutable.Stack
import scala.math

class Calculator (val variables: VariablesResolver.type) {

    private val vr = variables

    def calculate(expr: Expression): Constant[AnyVal] = {

        val it = expr.expression().iterator
        val c = Stack[ExpressionItem]()
        
        if (it.hasNext) 
            c.push(it.next)

        while(it.hasNext) {
            
            val i = it.next

            i match {
                case iInstr: Operator => {
                    val v = iInstr.operands match {
                        case 1 => List(c.pop().asInstanceOf[Constant[iInstr.Tin]])
                        case 2 => List(c.pop().asInstanceOf[Constant[iInstr.Tin]],c.pop().asInstanceOf[Constant[iInstr.Tin]])
                        case _ => null
                    }
                    calc(iInstr,v.reverse) match {
                        case Right(x) => c.push(x)
                        case Left(error) => { 
                                                println(s"An error ${error} occured") 
                                                c.clear()
                                            }
                    }

                }
                case iValue: Constant[_] => {
                    c.push(iValue)
                }
                case iVar: Variable[_] => {
                    c.push(Constant[AnyVal](vr.get(iVar.name).get))
                }
                case _ => {

                }
            }

        }
        
        c.pop().asInstanceOf[Constant[AnyVal]]
    }
    
    private def calc(op: Operator,vals: List[Constant[op.Tin]]): Either[OperatorError, Constant[op.Tout]] = {
        
        print(s"Calc ${op.symbol} on ${vals.head.value}")
        if (op.operands > 1 )
            println(s" and ${vals.last.value}")
        else
            println()

        op.operands match {
            case 1 => op.run(vals.head.asInstanceOf[Constant[op.Tin]])
            case 2 => op.run(vals.head.asInstanceOf[Constant[op.Tin]],vals.last.asInstanceOf[Constant[op.Tin]])
            case _ => Left(InvalidParametersError("Неподдерживаемое число параметров"))
        }
    }
}
