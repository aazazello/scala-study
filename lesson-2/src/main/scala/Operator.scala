package aazazello.expressioncalculator


case class InvalidParametersError(message: String) extends OperatorError
case class InvalidParameterTypesError(message: String) extends OperatorError
case class DivisionByZeroError(message: String) extends OperatorError

abstract class Operator (numberOfOperands: Int, operationSymbol: String, operationPriority: Int ) extends ExpressionItem {
    type Tin <: AnyVal
    type Tout <: AnyVal

    val operands = numberOfOperands
    val symbol = operationSymbol
    val priority = operationPriority

    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]]
    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]]

}

object OperatorSet {
    def all(): List[Operator] = { List( MinusOperator, PlusOperator, MultiplyOperator, DivideOperator, PowerOperator, EquivalentOperator, NonEquivalentOperator, GreateOperator, LessOperator, LogarithmNaturalOperator ) }
}