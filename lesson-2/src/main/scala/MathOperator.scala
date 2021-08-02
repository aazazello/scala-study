package aazazello.expressioncalculator

import scala.math

abstract class MathOperator (numberOfOperands: Int, operationSymbol: String, operationPriority: Int ) 
    extends Operator(numberOfOperands, operationSymbol, operationPriority) {
        type Tin = Double
        type Tout = Double

        override def run(single: Constant[Tin]): Either[OperatorError, Constant[Tout]]
        override def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]]
}

object PlusOperator extends MathOperator (2, "+", 50) {


    def run(single: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция сложения требует двух параметров"))}
    
    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value + right.value))
    }
}


object MinusOperator extends MathOperator (2, "-", 50) {

    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tin]] = { Left(InvalidParametersError("Операция вычитания требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value - right.value))
    }
}

object MultiplyOperator extends MathOperator (2, "*", 75) {

    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция умножения требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](left.value * right.value))
    }
}

object DivideOperator extends MathOperator (2, "/", 75) {

    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция деления требует двух параметров"))}
    
    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = {
        right.value match {
            case 0.0 => Left(DivisionByZeroError("Попытка деления на ноль"))
            case _ => Right(Constant[Tout](left.value / right.value))
        }
    }
}

object PowerOperator extends MathOperator(2, "^", 75) {

    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = { Left(InvalidParametersError("Операция возведения в степень требует двух параметров"))}

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Right(Constant[Tout](math.pow(left.value, right.value)))
   }
}

object LogarithmNaturalOperator extends MathOperator(1, "ln", 75) {

    def run(single:Constant[Tin]): Either[OperatorError, Constant[Tout]] = {
        Right(Constant[Tout](math.log(single.value)))
    }

    def run(left: Constant[Tin], right: Constant[Tin]): Either[OperatorError, Constant[Tout]] = { 
        Left(InvalidParametersError("Операция вычисления натурального логарифма требует одного параметра"))
    }
}