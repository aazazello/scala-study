package aazazello.expressioncalculator

abstract class LogicalOperator (numberOfOperands: Int, operationSymbol: String, operationPriority: Int ) 
    extends Operator(numberOfOperands, operationSymbol, operationPriority) {
        type Tin = Boolean
        type Tout = Boolean

        override def run(single: Constant[Tin]): Either[OperatorError, Constant[Tout]]
        override def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]]
}

object AndOperator extends LogicalOperator(2, "И", 1) {
    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция И требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value && right.value))
    }
}

object OrOperator extends LogicalOperator(2, "ИЛИ", 1) {
    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция ИЛИ требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value || right.value))
    }
}

object NotOperator extends LogicalOperator(1, "НЕ", 1) {
    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Right(Constant[Tout](!single.value)) }

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Left(InvalidParametersError("Операция НЕ требует одного параметра"))
    }
}