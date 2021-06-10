package aazazello.expressioncalculator

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.io.StdIn.readLine
import scala.util.Try

// Errors
abstract class ExpressionError
case class ExpressionParseError(e: String) extends ExpressionError
case class InvalidExpression(e: String) extends ExpressionError

//Expressions
class Expression() {
    
    private var src: String = ""
    private var postfixExpr: List[ExpressionItem] = null

    def expression(): Seq[ExpressionItem] = { postfixExpr.toSeq }

    def parse(expr: String): Either[ExpressionError, Expression] = {
        
        val stack = Stack[ExpressionItem]()
        src = expr
        var found = ListBuffer[ExpressionItem]()

        if (src.length > 0) { 

            // Split into items
            val items = split(expr)
            
            if (items.length > 2) {

                val e = items.map(a => parseItem(a)).filter(a => a != None)
                // build RPN
                e.foreach(i => {
                    
                    i match {
                        case Some(ii) => {

//                            print("Process: ")
//                            dump(ii)

                            ii match {
                                case dd: Divider if dd.value == "(" => {
//                                    print("push ")
//                                    dump(dd)

                                    stack.push(dd)
                                }
                                case dd: Divider if dd.value == ")" => {

                                    var e: ExpressionItem = null
//                                    println("pop until (")

                                    if (!stack.isEmpty) e = stack.pop()

                                    while(e != null) {
//                                        print("Popped1: ")
//                                        dump(e)
                                        e match {
                                            case odiv : Divider if odiv.value == "(" => {
                                                e = null
                                            }
                                            case cdiv : Divider if cdiv.value == ")" => {
                                                // Error in expression
                                                println("ERROR")
                                            }
                                            case op: Operator => {
//                                                print("out")
//                                                dump(op)
                                                found += op
                                                if (!stack.isEmpty) e = stack.pop() else e = null
                                            }
                                        }
                                    } 
                                }

                                case cc: Constant[_] => {
//                                    print("out")
//                                    dump(cc) 
                                    found += cc
                                }
                                case vv: Variable[_] => {
//                                    print("out")
//                                    dump(vv)
                                    found += vv
                                }
                                case op: Operator => {  
                                    var e: ExpressionItem = null
                                    if (!stack.isEmpty) e = stack.pop()

                                    while (e != null) {
//                                        print("Popped2")
//                                        dump(e)

                                        e match {
                                            case op1: Operator if op1.priority >= op.priority => {
//                                                print("out")
//                                                dump(op1)
                                                found += op1

                                                if (!stack.isEmpty) e = stack.pop() else e = null
                                            }
                                            case op2: Operator if op2.priority < op.priority => {
//                                                print("push")
//                                                dump(e)
                                                stack.push(e)

                                                e = null
                                                
                                            }
                                            case dd: Divider => {
                                                e = null
                                            }
                                        }
                                    }
//                                    print("push")
//                                    dump(op)
                                    stack.push(op)

                                }
                            }
                        }
                        case None => { println("Check filter, it shouldn't happens")}
                    } 
                })

                var item: ExpressionItem = null
                if (!stack.isEmpty) item = stack.pop()

                while( item != null ) {
                    item match {
                        case op: Operator => {
//                            print("out")
//                            dump(op)
                            found += op

                            if (!stack.isEmpty) item = stack.pop() else item = null
                        }   
                        case _ => { println(s"ERROR with ${item.toString()}")}
                    }                     
                }

                postfixExpr = List[ExpressionItem]() ++ found

                print("\nDUMP   ")
                
                postfixExpr.foreach(ei => {
                  ei.dump()  
                })
                println()

                Right(this)
            }
            else {
                Left(InvalidExpression("Expression must be at least of 3 items"))
            }
          
        }
        else
            Left(ExpressionParseError("No expression"))

    }

    def parseItem(item: String): Option[ExpressionItem] = {
        
        var result: Option[ExpressionItem] = None

        val ops = OperatorSet.all()

        item.trim() match {
            case "" => { result = None }
            case "(" | ")" => { 
                var r = new Divider()
                r.value = item
                result = Some(r)
            }
            case s if Try { s.toDouble }.toOption != None => {
                var c = new Constant[Double](s.toDouble)
                result = Some(c)
            }
            case s if ops.find(o => o.symbol == item) != None  => {
                result = ops.filter(o => o.symbol == item).headOption
            }
            case _ => {
                var v = new Variable[Double](item)
                result = Some(v)
            }
        }

        result
    }

    def split(expr:String): List[String] = {

        val sexpr: String = expr
        var res: ListBuffer[String] = ListBuffer.empty
        var t: String = ""
        val opcodes = OperatorSet.all().map(o => o.symbol)
        var i:Int = 0

        println(opcodes)
        while(i < sexpr.length()) {
            
            sexpr(i) match {
                case ' '  => { // space matters for logical expressions
                    if (t.length > 0) {
                        res += t
                        t = ""
                    }
                }
                case '(' | ')'  => {
                    if (t.length > 0) {
                        res += t
                        t = ""
                    }
                    res += sexpr(i).toString
                }
                case _ => {
                    val ss = sexpr.slice(i,sexpr.length())
            
                    //println (s"${i}:${ss}")
                    opcodes.filter(o => ss.startsWith(o)).sortBy(o => o.length) match {
                        case ops if ops.length > 0 => {
                            val op = ops(ops.length-1)

                            if (t.length > 0) {
                                res += t
                                t = ""
                            }
                            res +=  op
                            i += op.length() - 1                    
                        }
                        case nops if nops.length == 0 => {
                            t += sexpr.slice(i, i+1)
                        }
                        case _ => {
                            println("STRANGE opcodes filter result")
                        }
                    }
                }
            }
            
            i += 1
        }

        if (t.length > 0) res += t

        res.toList
    }

    def source: String = src

}
