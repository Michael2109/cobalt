<Class> package game;<Import> import javax.swing.JPanel;
<Import> import java.awt.Graphics;public final class Panel extends JPanel implements Runnable{
<ModifierBlock> <GlobalVar> public <Type> <Identifier> Thread <Identifier> thread=<NewClassInstance> new Thread(<This> this);public <Type> <Identifier> Thread <Identifier> thread(){return <NewClassInstance> new Thread(<This> this);} <GlobalVar> public <Type> <Identifier> int <Identifier> x=<ArithExpr> 50;public <Type> <Identifier> int <Identifier> x(){return <ArithExpr> 50;} <GlobalVar> public <Type> <Identifier> int <Identifier> y=<ArithExpr> 100;public <Type> <Identifier> int <Identifier> y(){return <ArithExpr> 100;} <GlobalVar> public <Type> <Identifier> int <Identifier> dx=<ArithExpr> 1;public <Type> <Identifier> int <Identifier> dx(){return <ArithExpr> 1;} <GlobalVar> public <Type> <Identifier> int <Identifier> dy=<ArithExpr> 1;public <Type> <Identifier> int <Identifier> dy(){return <ArithExpr> 1;}
<ModifierBlock> <GlobalVar> private <Type> <Identifier> boolean <Identifier> alive=<BooleanExpr> true;private <Type> <Identifier> boolean <Identifier> alive(){return <BooleanExpr> true;}private final <ClassParam> <Identifier> Game <Identifier> game;public Panel(<ClassParam> <Identifier> Game <Identifier> game){this.<Identifier> game=<Identifier> game;<ObjectMethodCall> thread.start();}<Function> public <Identifier> void update(){
<Reassign> x=<ArithExpr> x + dx;
<Reassign> y=<ArithExpr> y + dy;
<If> if(x < 0 || x > 750){
<Reassign> dx=<ArithExpr> dx * -1;}
<If> if(y < 0 || y > 550){
<Reassign> dy=<ArithExpr> dy * -1;}}
<Function> public <Identifier> void paint(<Identifier> Graphics <Argument> g){
<SuperMethodCall> super.paintComponent(<ArithExpr> g);
<ObjectMethodCall> g.drawRect(<ArithExpr> x, <ArithExpr> y, <ArithExpr> 50, <ArithExpr> 50);}
<Function> public <Identifier> void run(){
<Try> try{<While> while(<BooleanExpr> true){
<ObjectMethodCall> Thread.sleep(<ArithExpr> 7);
<FunctionCall> update();
<FunctionCall> repaint();}}
<Catch> catch(Exception e){}}}