package aazazello.expressioncalculator

abstract class ComparisionOperator (numberOfOperands: Int, operationSymbol: String, operationPriority: Int ) 
    extends Operator(numberOfOperands, operationSymbol, operationPriority) {
        type Tin = Double
        type Tout = Boolean

        override def run(single: Constant[Tin]): Either[OperatorError, Constant[Tout]]
        override def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]]
}

object NonEquivalentOperator extends ComparisionOperator(2,"<>", 10) {
    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция неравенства требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value != right.value))
    }
}

object EquivalentOperator extends ComparisionOperator(2,"=", 10) {
    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция равенства требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value == right.value))
    }
}

object GreateOperator extends ComparisionOperator(2,">", 10) {
    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция сравнения (больше) требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value > right.value))        
    }

}

object LessOperator extends ComparisionOperator(2,"<",10) {
    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция сравнения (меньше) требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value < right.value))
   }
}