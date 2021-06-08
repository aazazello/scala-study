package aazazello.expressioncalculator

import scala.math

case class InvalidParametersError(message: String) extends OperatorError
case class InvalidParameterTypesError(message: String) extends OperatorError
case class DivisionByZeroError(message: String) extends OperatorError

abstract class Operator (numberOfOperands: Int, operationSymbol: String, operationPriority: Int ) extends ExpressionItem {
    val operands = numberOfOperands
    val symbol = operationSymbol
    val priority = operationPriority

    def run(single:AnyVal): Either[OperatorError, AnyVal]
    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal]

}

object PlusOperator extends Operator (2, "+", 2) {


    def run(single:AnyVal): Either[OperatorError, AnyVal] = { Left(InvalidParametersError("Операция сложения требует двух параметров"))}

    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = { 
        (left, right) match {
            case (_: Int, _: Int) => Right(left.asInstanceOf[Int] + right.asInstanceOf[Int])
            case ((_: Int, _: Long) | (_: Long, _: Int) | (_: Long, _: Long)) => Right(left.asInstanceOf[Long] + right.asInstanceOf[Long])
            case (_: Float, _: Float) => Right(left.asInstanceOf[Float] + right.asInstanceOf[Float])
            case ((_: Float, _: Float) | (_: Float, _: Double) | (_: Double, _: Float) | (_: Double, _: Double)) => Right(left.asInstanceOf[Double] + right.asInstanceOf[Double])
            case _ => Left(InvalidParameterTypesError("Некорректные параметры операции сложения"))
        }
    }
}

object MinusOperator extends Operator (2, "-", 2) {

    def run(single:AnyVal): Either[OperatorError, AnyVal] = { Left(InvalidParametersError("Операция вычитания требует двух параметров"))}

    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = { 
        (left, right) match {
            case (_: Int, _: Int) => Right(left.asInstanceOf[Int] - right.asInstanceOf[Int])
            case ((_: Int, _: Long) | (_: Long, _: Int) | (_: Long, _: Long)) => Right(left.asInstanceOf[Long] - right.asInstanceOf[Long])
            case (_: Float, _: Float) => Right(left.asInstanceOf[Float] - right.asInstanceOf[Float])
            case ((_: Float, _: Float) | (_: Float, _: Double) | (_: Double, _: Float) | (_: Double, _: Double)) => Right(left.asInstanceOf[Double] - right.asInstanceOf[Double])
            case _ => Left(InvalidParameterTypesError("Некорректные параметры операции вычитания"))
        }
    }
}

object MultiplyOperator extends Operator (2, "*", 3) {

    def run(single:AnyVal): Either[OperatorError, AnyVal] = { Left(InvalidParametersError("Операция умножения требует двух параметров"))}

    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = { 
        (left, right) match {
            case ((_: Int, _: Int) | (_: Int, _: Long) | (_: Long, _: Int) | (_: Long, _: Long)) => Right(left.asInstanceOf[Long] * right.asInstanceOf[Long])
            case ((_: Float, _: Float) | (_: Float, _: Float) | (_: Float, _: Double) | (_: Double, _: Float) | (_: Double, _: Double)) => Right(left.asInstanceOf[Double] * right.asInstanceOf[Double])
            case _ => Left(InvalidParameterTypesError("Некорректные параметры операции вычитания"))
        }
    }
}

object DivideOperator extends Operator (2, "/", 3) {

    def run(single:AnyVal): Either[OperatorError, AnyVal] = { Left(InvalidParametersError("Операция деления требует двух параметров"))}
    
    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = {
        (left, right) match {
            case (_,0) => Left(DivisionByZeroError("Попытка деления на ноль"))
            case ((_: Int, _: Int) | (_: Int, _: Long) | (_: Long, _: Int) | (_: Long, _: Long)) => Right(left.asInstanceOf[Long] / right.asInstanceOf[Long])
            case ((_: Float, _: Float) | (_: Float, _: Float) | (_: Float, _: Double) | (_: Double, _: Float) | (_: Double, _: Double)) => Right(left.asInstanceOf[Double] / right.asInstanceOf[Double])
            case _ => Left(InvalidParameterTypesError("Некорректные параметры операции вычитания"))
        }
    }
}

object PowerOperator extends Operator(2, "^", 3) {

    def run(single:AnyVal): Either[OperatorError, AnyVal] = { Left(InvalidParametersError("Операция возведения в степень требует двух параметров"))}

    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = { 
        (left, right) match {
            case ((_: Int, _: Int) | (_: Int, _: Long) | (_: Long, _: Int) | (_: Long, _: Long)) => Right(math.pow(left.asInstanceOf[Double],right.asInstanceOf[Double]).asInstanceOf[Long])
            case ((_: Float, _: Float) | (_: Float, _: Float) | (_: Float, _: Double) | (_: Double, _: Float) | (_: Double, _: Double)) => Right(math.pow(left.asInstanceOf[Double],right.asInstanceOf[Double]))
            case _ => Left(InvalidParameterTypesError("Некорректные параметры операции вычитания"))
        }
    }
}

object LogarithmNaturalOperator extends Operator(1, "ln",3) {

    def run(single:AnyVal): Either[OperatorError, AnyVal] = {
        single match {
            case (_: Int | _: Long ) => Right(math.log(single.asInstanceOf[Double]))
            case (_: Float | _: Double) => Right(math.log(single.asInstanceOf[Double]))
            case _ => Left(InvalidParameterTypesError("Некорректный параметр операции вычисления натурального логарифма"))
        }
    }

    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = { 
        Left(InvalidParametersError("Операция вычисления натурального логарифма требует одного параметра"))
    }
}

object NonEquivalentOperator extends Operator(2,"<>", 1) {
    def run(single:AnyVal): Either[OperatorError, AnyVal] = { Left(InvalidParametersError("Операция неравенства требует двух параметров"))}

    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = { 
        (left, right) match {
            case ((_: Int, _: Int) | (_: Int, _: Long) | (_: Long, _: Int) | (_: Long, _: Long)) => Right(if (left != right) 1 else 0)
            case ((_: Float, _: Float) | (_: Float, _: Float) | (_: Float, _: Double) | (_: Double, _: Float) | (_: Double, _: Double)) => Right(if (left != right) 1 else 0)
            case _ => Left(InvalidParameterTypesError("Некорректные параметры операции неравенства"))
        }
    }

}

object GreateOperator extends Operator(2,">", 1) {
    def run(single:AnyVal): Either[OperatorError, AnyVal] = { Left(InvalidParametersError("Операция сравнения (больше) требует двух параметров"))}

    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = { 
        (left, right) match {
            case ((_: Int, _: Int) | (_: Int, _: Long) | (_: Long, _: Int) | (_: Long, _: Long)) => Right(if (left.asInstanceOf[Long] > right.asInstanceOf[Long]) 1 else 0)
            case ((_: Float, _: Float) | (_: Float, _: Float) | (_: Float, _: Double) | (_: Double, _: Float) | (_: Double, _: Double)) => Right(if (left.asInstanceOf[Double] > right.asInstanceOf[Double]) 1 else 0)
            case _ => Left(InvalidParameterTypesError("Некорректные параметры операции сравнения"))
        }
    }

}

object LessOperator extends Operator(2,"<",1) {
    def run(single:AnyVal): Either[OperatorError, AnyVal] = { Left(InvalidParametersError("Операция сравнения (меньше) требует двух параметров"))}

    def run(left: AnyVal, right: AnyVal): Either[OperatorError, AnyVal] = { 
        (left, right) match {
            case ((_: Int, _: Int) | (_: Int, _: Long) | (_: Long, _: Int) | (_: Long, _: Long)) => Right(if (left.asInstanceOf[Long] < right.asInstanceOf[Long]) 1 else 0)
            case ((_: Float, _: Float) | (_: Float, _: Float) | (_: Float, _: Double) | (_: Double, _: Float) | (_: Double, _: Double)) => Right(if (left.asInstanceOf[Double] < right.asInstanceOf[Double]) 1 else 0)
            case _ => Left(InvalidParameterTypesError("Некорректные параметры операции сравнения"))
        }
    }
}
object OperatorSet {
    def all(): List[Operator] = { List( MinusOperator, PlusOperator, MultiplyOperator, DivideOperator, PowerOperator, NonEquivalentOperator, GreateOperator, LessOperator, LogarithmNaturalOperator ) }
}