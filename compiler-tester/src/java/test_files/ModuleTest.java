<Module> package test_files;<Import> import java.util.Random;public final class ModuleTest{
final Random random= new Random();<Function> public static <Identifier> void testFunction(<Identifier> int <Argument> x){
<If> if(x < 50){
<Print> System.out.println(<ArithExpr> x);
<Assign> <Type> <Identifier> int <Identifier> nextX=<ArithExpr> x + 1 * 2 - 3 / 2 + 5;
<FunctionCall> testFunction(<Argument> nextX);}
<Else>  else {
<Assign> <Type> <Identifier> int <Identifier> last=<ArithExpr> 1000;
<Print> System.out.println(<ArithExpr> last);}}
<MainFunction> public static <Identifier> void main(<Identifier> String <Argument> args){
<Assign> <Type> <Identifier> int <Identifier> x=<ArithExpr> 3;
<Assign> <Type> <Identifier> String <Identifier> example1=<StringLiteral> "Example";
<Print> System.out.println(<ArithExpr> example1);
<Assign> <Type> <Identifier> int <Identifier> y=<ArithExpr> 10;
<Assign> <Type> <Identifier> int <Identifier> z=<ArithExpr> 15;
<ArrayAssignment> <ArrayDef> int[] arrTest=<ArrayValues> {x, y, z};
<Print> System.out.println(<ArithExpr> arrTest);
<FunctionCall> testFunction(<Argument> x);
<Assign> <Type> <Identifier> String <Identifier> stringTest=<StringLiteral> "Hello world";
<Assign> <Type> <Identifier> String <Identifier> otherStringTest=<StringLiteral> "Test";
<If> if(x < 4){
<Print> System.out.println(<ArithExpr> stringTest);}
<ElseIf>  else if(x > 10){
<Print> System.out.println(<ArithExpr> stringTest);}
<Else>  else {
<Print> System.out.println(<ArithExpr> otherStringTest);}}}