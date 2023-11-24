fun println(a : String) : Unit {
	return;
}

fun main() :Unit{

    var x : Int = 3;

    when(x) {
        1 -> println("x is one");
        2 -> println("x in two");
        else -> {
            println("x is something else");
        }
    }
}
