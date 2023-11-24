fun function(a: Int, b: Int) : Unit{

}

class Class1{
    var first : Int = 0;
    var second: Double = 0.0;

    fun classFunction(a: Int, b: Int): Int{
        return a + b;
    };
}

fun main () :Unit{
    var a : Int = 1;
    var b : Int = 2;

    function(a, 5);
    var c : Class1 = Class1();
    a = c.first;
    var d : Int = 12;

    a = a++;
    a = a--;

    a = ++a;
    a = --a;
    a = -a;

    a = b*b;
    a = b+b;
    a = b-b;
    var e : Int = b/b;
    var f : Boolean = a<b;
    f = a>b;
    f = a<=b;
    f = a>=b;
    f = a==b;
    f = a!=b;
    f = true && false;
    f = true || false;
}
