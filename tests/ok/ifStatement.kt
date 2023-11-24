fun main() :Unit{
    var a : Int = 1;
    var b : Int = 2;
    if (true) { a = b; } else { b = a;  }

    if(a != b){
        b = a;
    }
}
