class Class1{
    var a : Int = 0;
    var b : Int = 0;
    var good : Boolean = true;

    fun function() :Unit{ };
}


fun main() :Unit{
    var x : Class1 = Class1();
    var b : Int = x.a;
    when (x.good)
    {
        true -> true;
        false -> false;
    }
}
