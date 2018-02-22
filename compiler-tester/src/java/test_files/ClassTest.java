<Class> package test_files;<Import> import javax.swing.JPanel;
<Import> import java.awt.Graphics;public final class ClassTest extends JPanel implements Runnable{
<ModifierBlock> <GlobalVar> public <Type> <Identifier> Thread <Identifier> thread=<NewClassInstance> new Thread(<This> this);public <Type> <Identifier> Thread <Identifier> thread(){return <NewClassInstance> new Thread(<This> this);}public ClassTest(){}<Function> public <Identifier> void update(<Identifier> int <Argument> x){
<If> if(x < 50){
<Print> System.out.println(<ArithExpr> x);
<Assign> <Type> <Identifier> int <Identifier> nextX=<ArithExpr> x + 1 * 2 - 3 / 2 + 5;}
<Else>  else {
<Assign> <Type> <Identifier> int <Identifier> last=<ArithExpr> 1000;
<Print> System.out.println(<ArithExpr> last);}}
<Function> public <Identifier> void paint(<Identifier> Graphics <Argument> g){
<Assign> <Type> <Identifier> int <Identifier> x=<ArithExpr> 3;}
<Function> public <Identifier> void run(){
<Assign> <Type> <Identifier> int <Identifier> x=<ArithExpr> 10 + 1;}}