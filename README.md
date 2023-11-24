# Table Of Contents
1. [Building the compiler](#building-the-compiler)
2. [Definition of a Kotlin Subset](#definition-of-a-kotlin-subset)
3. [Short Explanation of Test Cases](#tests)

# Building the Compiler

### Build without testing
1. Clone the repository
2. Change directory into `group03/P4/`
3. Build using `make build`
4. use the compiler with `./compiler [FILENAME].kt` 

### Build with testing
1. Clone the repository
2. Change directory into `group03/P4/`
3. Build using `make test`
4. Outputs will be written into `tests.txt`

# Definition of a Kotlin Subset

## Program

A Program is a list of Definitions. A program may also contain comments, which are ignored by the parser.

## Definition

A Definition can be:

### Function Definitions

A function definition is indicated by the `fun` keyword. It has a name, a return type, an argument list, and a body.

The argument list is written within parantheses `()` (see below).

The return Type of a function is written after the parantheses `()` seperated by a colon `:`.

The body of a function is written within curly braces `{}`.

the `return` keyword is used to exit or return something from a function.

```kotlin
fun foo (): Unit {
    return println("Hello, world!");
}

fun bar (a : Int, b : Int) : Int {
    return a + b;
}

```

### Class Definitions

A class definition is a name and a list of variable and function declarations.

The class definition is declared by using the keyword `class` followed by the name of the class starting with a capital letter followed by a Block `{}` The class properties (variable declarations) and functions are declared in the block. Since there is no explicit constructor the properties must be initialized.

```kotlin
class ClassName {
    var foo : Int = 1;
    var bar : Double = 1.1;
    val baz : Int = 2;

    fun fooFunc (a : Int, b : Int, c : Int = 7) {...};
    fun barFunc (a : Int, b : Int, c : Int = 7) {...};
}
```

The properties and functions inside the block of the class are seperated by a semicolon.

When a class is defined, an implicit constructor-function with the classname with return type of the class is declared aswell. 

### Argument Lists

An argument list is a comma-seperated `,` list of argument declarations.

It is written within parentheses `()`.

An argument declaration is an Identifier followed by a Type seperated by a colon `:`.

Arguments can optionally have default values set by appending `= [VALUE]` to the type.

Examples for Argument Declarations:

```kotlin
fun foo (a : Int, b : Int, c : Int = 7): Unit {...}

```

## Statements

Any expression followed by semicolon `;` can be used as a statement. 

Any variable declaration followed by a semicolon `;` can be used as a statement. 


### Variable Declarations

Variable declarations are indicated by either the `var` keyword for mutable variables or the `val` keyword for immutable variables (similar to the `const` in c++) but other than that are like argument declarations. 

#### Nullable variables

Per default variables cannot have the value `null`. Variables, that can be assigned the value `null` are specified when declared using the `?` right after the specified type.

examples for variable declarations: 

```kotlin
var a : Int;
val b : Int = 3;
var c : Int = b;

var nullable : Int? = null;     // ok
var non-nullable : Int = null;  // not ok
```

### Return Statements

There can also be statements to return expressions. 

The type of the returned expression must be the same as the return type of the function in which it occurs. In case of functions with `Unit` as return type, the return statement has no argument.

```kotlin
return i + 9;
return;
``` 

### If Statements

an if-statement is the keyword `if` followed by a condition in parentheses `()` followed by a statement and optionally by the keyword `else` and another statement. 

The condition is a boolean expression.

Branches of an if expression can be blocks, for example:

```kotlin
if (a > b) {
    print("Choose a");
} else {
    print("Choose b");
}
```

### While-Statements

A while loop is indicated by the `while` keyword followed by an boolean expression in parentheses `()` followed by a statement.

```kotlin 
var i = 100
while (i != 0) {
    sum += i;   
    --i;
}
```

### For-Statements

A for loop is the keyword `for` followed by a variable identifier and a expression seperated by the keyword `in` inside of parantheses `()` followed by a statement.

The expression can either be a Range or a String.

A range are two expressions seperated by `..`. They are evaluated by the TypeChecker as a List of Integers.


For the Typechecker the variable is seen as a variable declaration of type `Int` inside of a new block, where type is of.

```kotlin
for (a in 2..4) println(a);

for (character in "Hello")
{
    println(character);
}
```

### When-Statements

`when` followed by an expression inside of parentheses `()` indicates a when-statement. A when statement consists of multiple branches inside of curly braces `{}`. Where branches consist of an expression followed by a `->` and a statements afterwards. Branches are seperated by semi-colon `;`.

If none of the branches conditions are satisfied an optionally lastly defined else-branch is evaluated.

```kotlin
when(x) {
    1 -> println("x is one");
    2 -> println("x in two");
    else -> {
        println("x is something else");
    }
}
```

The Typechecker has to make sure, there only exists one `else` Branch.

## Expressions 

| level | expression forms                  | assoc | explanation               | type                  |    
| ----- | --------------------------------- | ----- | ------------------------- | --------------------- |
|16     |literal                            |       |atomic expression          |literal type           |
|16     |identifier                         |       |variable                   |declared type          |
|16     |`f(e, ..., e)`                     |left   |function call              |function type signature|
|15     |`e..e`                             |left   |range                      |list of integer        |
|14     |`e.e`                              |left   |class projection           |type of the field `id` |   
|14     |`e++`, `e--`                       |left   |in/decrement               |operand type           |
|13     |`++e`, `--e`                       |right  |in/decrement               |operand type           |
|13     |`+e`, `-e`                         |right  |unary plus/minus           |operand type           |
|12     |`e*e`, `e/e`                       |left   |multiplication, division   |operand type           |
|11     |`e+e`, `e-e`                       |left   |addition, subtraction      |operand type           |
|9      |`e<e`, `e>e`, `e<=e`, `e>=e`       |left   |comparison                 |bool                   |
|8      |`e==e`, `e!=e`                     |left   |(in)equality               |bool                   |
|4      |`e&&e`                             |left   |conjunction                |bool                   |
|3      |`e\|\|e`                           |left   |disjunction                |bool                   |
|2      |`e=e`                              |right  |assignment                 |Type of LHS            |
|2      |`{id : type, ... -> e}`            |right  |lambda-expression          |function signature     |

### Lambda Expressions

Lambda expressions are anonymous functions. Specified inside of curly braces `{}`
They can be assigned to variables or used by themselves.


```kotlin
var a : (Int, Int) -> Int = {i: Int, j: Int -> i * j};
```

The return type is the infered type of the right expression.

## Types

### Basic Types

- `Int`
- `Double`
- `Bool`
- `String`

### Other Types

- `IntList`
- `Unit`

The type `Unit` is similar to `void` in c++.

The type `IntList` is necesarry to Typecheck the ForStatement

### Function Types

The Signature of a function can be Specified using the `->` Operator, 
where the argument type of the function is inside of braces `()` on the left of `->`, and the return type on the right.

Multiple argument types need to be seperated by a comma `,`.

```kotlin
var a : (Int) -> Int
var b : (Int, String) -> Int 
```

#### Return Type

The Return Type of a function is specified after the argument list with a `:` inbetween

```kotlin
fun foo (a : Int) : Int {...}

```


## Literals

We include integer literals, floating point literals and the value `null` as a literal.

## Identifiers

An identifier is a letter followed by a list of letters, digits and underscores.

## Comments

There are two kinds of comments
 - anything between tokens `/*` and `*/`
 - anything from token `//` to the end of the line

# Tests

## Legal Test Programs

| Error | Filename              | Explanation |
| ----- | ---------             | ----------- |
| OK    | tests/ok/ForStm.kt    | Test `For-Loop` inside of `Functions` |
| OK    | ../argumentList.kt    | Tests `Function Definitions` |
| OK    | ../class.kt           | Tests `Classes`, `Properties` and `Member Functions`, `Constructor`, `Class Projection` |
| OK    | ../constructor.kt     | Tests `Classes`, `Properties` and `Member Functions`, `Constructor` |
| OK    | ../expressions.kt     | Tests `Expressions` |
| OK    | ../forStatement.kt    | Tests `For-Loop` |
| OK    | ../forString.kt       | Tests `For-Loop` with String as range |
| OK    | ../helloWorld.kt      | Tests `String Literal` and `Function Calls` |
| OK    | ../ifStatement.kt     | Tests `If-Else Statements` and a few `Expressions`|
| OK    | ../lambda_assign.kt   | Tests `Lambda-Expressions` and the `Assignment` of those and variables uf function Types |
| OK    | ../lambdaExpression.kt| Tests `Lambda-Expressions` and the `Assignment` of those |
| OK    | ../mainfunc.kt        | Tests an Empty `Main Function` |
| OK    | ../manyFunc.kt        | Tests multiple Empty `Function Definitions`|
| OK    | ../mutibility.kt      | Tests mutabl`var` and imutable variables `val`|
| OK    | ../nullableVar.kt     | Tests `Null Safety` and `Ǹullable Variables` |
| OK    | ../returnStatement.kt | Tests `return Statements` and `Function Calls`|
| OK    | ../varDec.kt          | Tests `Variable Declarations`, Basic `Types` and `Function Types`|
| OK    | ../whenStatement.kt   | Tests `Ẁhen Statement` with `else` case |
| OK    | ../whileStatement.kt  | Tests `While Statement` |

## Syntactically Illegal Test Programs

| Error | Filename              | Explanation |
| ----- | ---------             | ----------- |
| Syntax Error | tests/parser/error_funcDeclaration.kt| Error `Function Declaration body`|
| Syntax Error | ../errCmt.kt                         | Unclosed comment |
| Syntax Error | ../error_comment.kt                  | Usage of wrong nested comment|
| Syntax Error | ../error_forStm.kt                   | For loop must look like `for(var in range)`|
| Syntax Error | ../error_funcDeclaration2.kt         | Missing parentheses after main `()`|
| Syntax Error | ../Error_funcDeclaration2.kt         |Function body is enclosed in `[]` instead of `{}`|
| Syntax Error | ../error_lambdaExpression.kt         | The body of the lambda expression comes after `=>` instead of `->`| 
| Syntax Error | ../error_nullable.kt                 | The nullable indicator `?` comes after the type for nullable variables|
| Syntax Error | ../error_varDec.kt                   | Missing type `Int` in variable declaration |
| Syntax Error | ../error_returnStatement.kt          | Missing semicolon `;` after return statement |
| Syntax Error | ../error_argumentDeclaration.kt      | Argument list must be separated with `,` and not `;`|
| Syntax Error | ../error_forRange.kt                 | Keyword inside instead of `in`|


## Semantically Illegal Test Programs

| Error | Filename              | Explanation |
| ----- | ---------             | ----------- |
| Semantic Error | tests/typechecker/block.kt | Cannot call the variable outside the block |
| Semantic Error | ../classSameFun.kt         | Multiple declaration of functions with the same name in class |
| Semantic Error | ../classSameVar.kt         | Multiple declaration of variables with the same name in class |
| Semantic Error | ../lambda_ass_wrong_type.kt| Typemismatch in lamda expression |
| Semantic Error | ../lambda_init_wrong_type.kt | Typemismatch in lamda expression |
| Semantic Error | ../mutability.kt           | multiple assignments of immutable variables not allowed |
| Semantic Error | ../sameclass.kt            | multiple declarations of class with same id |
| Semantic Error | ../VarDecl.kt              | Variable not declared |
| Semantic Error | ../var_name_func.kt        | Function with same name as a variable inside a class not allowed |
| Semantic Error | ../when_mult_else.kt       | multiple else branches inside of when statement |
| Semantic Error | ../OperatorErr.kt          | Operator "+" cannot be applied to Bool value |
| Semantic Error | ../when_wrong_type.kt      | Typemismatch in when statement |
| Semantic Error | ../typeReturn.kt           | Return the wrong type |
| Semantic Error | ../numberofArgs.kt         | Wrong number of arguments were passed to the function call |
| Semantic Error | ../samefunc.kt             | Define 2 functions in a same class with same name and arguments |
| Semantic Error | ../wrongOperator.kt        | Assignment operator used instead of the equality operator in the if statement condition |
