package aazazello.expressioncalculator

// Operands
abstract class Operand [T] () extends ExpressionItem { 
    def value: T
    def value_= (arg: T): Unit
}

class Variable [T] (varName: String) extends Operand [T] () {
    private var _val: T = _

    val name: String = varName

    def value: T = _val 
    def value_= (arg: T) = _val = arg
}

class Constant [T] (constValue: T) extends Operand [T] () {
    override def value = {constValue}
    override def value_= (arg: T) = {}
}
