package aazazello.expressioncalculator

trait ExpressionItem {

    def dump(): Unit = {
        this match {
            case op: Operator => {
               print(s"${op.symbol}->") 
            }
            case cc: Constant[_] => {
                print(s"${cc.value}->")
            }
            case vv: Variable[_] => {
                print(s"${vv.name}->")
            }
            case dd: Divider => {
                print(s"${dd.value}->")
            }
        }
        //readLine()
    }

}

class Divider () extends ExpressionItem { 
    private var divider: String = _
    
    def value: String = {divider}
    def value_= (arg: String) = { divider = arg }
}

