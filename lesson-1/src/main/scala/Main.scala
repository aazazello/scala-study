object Lesson_1 {

    case class BasicThing(name: String, timestamp: Long)

    @main def start (args: String): Unit = {

        var newThing = BasicThing("I'm awesome " + args + " at", System.currentTimeMillis)

        println("Hello world!")
        println(s"My message is ${newThing.name}: ${newThing.timestamp}")
    }

}