package test_files;import java.util.Random;public final class ModuleTest{
final Random random= new Random();class A{}final class B extends A { final String var0; public B(final String var0){this.var0=var0;} } final class C extends A { final String var0; public C(final String var0){this.var0=var0;} } final class D extends A { final Integer var0; public D(final Integer var0){this.var0=var0;} }
public static void testFunction(int x){
if(x < 50){
System.out.println(x);
int nextX=x + 1 * 2 - 3 / 2 + 5;
testFunction(nextX);}
 else {
int last=1000;
System.out.println(last);}}
public static IO main(String args){
int x=3;
String example1="Example";
System.out.println(example1);
int y=10;
int z=15;
int[] arrTest={x, y, z};
System.out.println(arrTest);
ModuleTest.testFunction(x);
String stringTest="Hello world";
String otherStringTest="Test";
if(x < 4){
System.out.println(stringTest);}
 else if(x > 10){
System.out.println(stringTest);}
 else {
System.out.println(otherStringTest);}}}