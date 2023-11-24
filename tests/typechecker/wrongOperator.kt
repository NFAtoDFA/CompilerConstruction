fun main() : Unit{
    var a : Int = 1;
    var b : Int = 2;
    if(a = b){
        a = b;
    } else {
        b = a;
    }

    if(a != b){
        b = a;
    }
}

// assignment operator used instead of the equality operator in the if statement condition
