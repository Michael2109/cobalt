package test_files;import java.util.Random;public final class ModuleTest{
final Random random= new Random();public static void testFunction(int x){
if(x < 50){
System.out.println(x);
int nextX=x + 1 * 2 - 3 / 2 + 5;
testFunction(nextX);}
 else {
int last=1000;
System.out.println(last);}}
public static void main(String args){
int x=3;
String example1="Example";
System.out.println(example1);
int y=10;
int z=15;
int[] arrTest={x, y, z};
System.out.println(arrTest);
testFunction(x);
String stringTest="Hello world";
String otherStringTest="Test";
if(x < 4){
System.out.println(stringTest);}
 else if(x > 10){
System.out.println(stringTest);}
 else {
System.out.println(otherStringTest);}}}