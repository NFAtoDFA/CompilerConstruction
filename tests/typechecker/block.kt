fun main()  : Unit{
    {
        var a : Int = 2;
        {
            var a : Int = 0;
            var b : Int = 1;
        }
        a;
        b;
    }
}
//Cannot call the variable outside the block
