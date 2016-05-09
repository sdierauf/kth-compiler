package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._
import slacc.ast.Printer


object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._


    // Step 1: Collect symbols in declarations
    var globalScope = new GlobalScope()
    var unusedMethodArgs: Set[String] = Set()
    var unusedClassVars: Set[String] = Set()
    var unusedMethodVars: Set[String] = Set()
    var classNameToClass: Map[String, ClassDecl] = Map()
    var collectedClasses: Set[String] = Set()

    def useVariable(variable: String): Unit ={
      unusedMethodArgs = unusedMethodArgs - variable
      unusedClassVars = unusedClassVars - variable
      unusedMethodVars = unusedMethodVars - variable
    }


    def addClassSymbol(klass: ClassDecl, scope: GlobalScope): Unit = {
      val className = klass.id.value
      val symbol = new ClassSymbol(className).setPos(klass)
      scope.lookupClass(className) match {
        case Some(v) => error("collectClassDecl2: already a class with that name defined in the scope", v);
        case None => scope.classes += (className -> symbol)
      }
      classNameToClass += (className -> klass)
    }

    def checkParent(klass: ClassDecl, scope: GlobalScope): Unit = {
      val symbolOption: Option[ClassSymbol] = scope.lookupClass(klass.id.value)
      val symbol = symbolOption.get
      klass.parent match {
        case Some(p) => {
          val parent = scope.lookupClass(p.value)
          if (parent.isEmpty) {
            error("checkParent: no matching symbol for parent of class " + klass.id.value, symbol)
          }
          symbol.parent = parent
          // check for inheritance cycle
          if (hasInheritanceCycle(symbol, scope)) {
            error("checkParent: symbol " + klass.id.value + " was part of an inheritance cycle!!", symbol)
          }
          // collect parent's methods and vars before collecting this one
          checkParent(classNameToClass.get(parent.get.name).get, scope)
          collectClassDecl(klass, scope)
        }
        case None => collectClassDecl(klass, scope)
      }
    }


    def collectMainMethod(n: MainMethod, scope: GlobalScope): Unit = {
      if (scope.mainClass != null) {
        error("collectMainMethod: Main class already declared", n)
      }
      scope.mainClass = new ClassSymbol(n.id.value).setPos(n)
      scope.classes += (n.id.value -> scope.mainClass)
      collectMethodDecl(n.main, scope.mainClass)
    }


    def collectClassDecl(klass: ClassDecl, scope: GlobalScope): Unit = {
      val className = klass.id.value
      println("collecting " + className)
      if (collectedClasses.contains(className)) {
        return
      } else {
        collectedClasses += className
      }
      val symbol = scope.lookupClass(className).get
      klass.vars.foreach(v => collectVarDecl(v, symbol))
      klass.methods.foreach(m => collectMethodDecl(m , symbol))
    }

    def hasInheritanceCycle(symbol: ClassSymbol, scope: GlobalScope): Boolean = {
      var visited: Set[Int] = Set()
      visited = visited + symbol.id
      var currentClass = symbol.parent
      while (currentClass.isDefined) {
        currentClass match {
          case Some(k) => {
            if (visited.contains(k.id)) {
              return true
            } else {
              visited = visited + k.id
              currentClass = k.parent
            }
          }
        }
      }
      false
    }

    def collectVarDecl(n: VarDecl, scope: Symbol): Unit = {
      val varName = n.id.value
      val symbol = new VariableSymbol(varName).setPos(n)
      scope match {
        case s: ClassSymbol => {
          if (s.lookupVar(varName).isDefined) {
            error("collectVarDecl: class " + s.name + " already had a variable named " + varName, n)
          }
          s.members += (varName -> symbol)
        }
        case s: MethodSymbol => {
          // it's ok if has same name as a class variable
          // not ok if it's in declared vars
          if (s.members.get(varName).isDefined || s.params.get(varName).isDefined) {
            error("collectVarDecl: method " + s.name + " already had a variable named " + varName, n)
          }
          s.members += (varName -> symbol)

        }
        case _ => {
          error("Collected a variable not in a Class or MethodSymbol!!")
        }
      }
    }

    def collectMethodDecl(method: MethodDecl, scope: ClassSymbol): Unit = {
      val methodName = method.id.value
      val symbol = new MethodSymbol(methodName, scope).setPos(method)
      def addMethod(): Unit = {
        scope.methods += (methodName -> symbol)
        method.args.foreach(arg => collectFormal(arg, symbol))
        method.vars.foreach(v => collectVarDecl(v, symbol))
      }
      // TODO: Check parameters of overriding methods
      if (scope.methods.contains(methodName)) {
        error("collectMethodDecl: method " + methodName + " was already defined!", method)
      } else {
        scope.lookupMethod(methodName) match {
          case Some(parentMethod) => {
            if (parentMethod.argList.length != method.args.length) error("Overriding method with unequal number of arguments in " + methodName, method))

            else addMethod()
          }
          case None => addMethod()
        }
      }
    }

    def attachExpr(expr: ExprTree, symbol: MethodSymbol): Unit = {
//      case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree
//      case class ArrayLength(arr: ExprTree) extends ExprTree
      expr match {
        // case Trees(lhs : ExprTree, rhs : ExprTree) => idk if something like this would work
        case t : And => {
          attachExpr(t.lhs, symbol)
          attachExpr(t.rhs, symbol)
        } case t : Or => {
          attachExpr(t.lhs, symbol)
          attachExpr(t.rhs, symbol)
        } case t : Plus => {
          attachExpr(t.lhs, symbol)
          attachExpr(t.rhs, symbol)
        } case t : Minus => {
          attachExpr(t.lhs, symbol)
          attachExpr(t.rhs, symbol)
        } case t : Times => {
          attachExpr(t.lhs, symbol)
          attachExpr(t.rhs, symbol)
        } case t : Div => {
          attachExpr(t.lhs, symbol)
          attachExpr(t.rhs, symbol)
        } case t : LessThan => {
          attachExpr(t.lhs, symbol)
          attachExpr(t.rhs, symbol)
        } case t : Equals => {
          attachExpr(t.lhs, symbol)
          attachExpr(t.rhs, symbol)
        } case b : Block => {
          b.exprs.foreach(e => attachExpr(e, symbol))
        } case ifthen : If => {
          attachExpr(ifthen.expr, symbol)
          attachExpr(ifthen.thn, symbol)
          ifthen.els match {
            case Some(e3) => attachExpr(e3, symbol)
            case None =>
          }
        } case w : While => {
          attachExpr(w.body, symbol)
          attachExpr(w.cond, symbol)
        } case p : Println => {
          attachExpr(p.expr, symbol)
        } case s : Strof => {
          attachExpr(s.expr, symbol)
        }
        case a : Assign => {
            // need to check that the variable being assigned has been declared
           // or do we do that somewhere else
          attachExpr(a.id, symbol)
          attachExpr(a.expr, symbol)
        } case i : Identifier => {
            // need to look up that identifier has been declared
            symbol.lookupVar(i.value) match {
              case Some(s) => i.setSymbol(s); useVariable(i.value);
              case None => {
                globalScope.lookupClass(i.value) match {
                  case Some(s) => i.setSymbol(s); useVariable(i.value);
                  case None => return; sys.error("collectExpressionDecl: No match for " + i.value)
                }
              }
            }
        }
        case m :MethodCall => {
          attachExpr(m.obj, symbol)
          attachExpr(m.meth, symbol)
          m.args.foreach(ar => attachExpr(ar, symbol))
        }
        case e: ArrayRead => {
          attachExpr(e.arr, symbol)
          attachExpr(e.index, symbol)
        }
        case e: ArrayLength => {
          attachExpr(e.arr, symbol)
        }
        case e: ArrayAssign => {
          attachExpr(e.id, symbol)
          attachExpr(e.expr, symbol)
          attachExpr(e.index, symbol)
        }
        case n: New => {
          attachTypeTree(n.tpe)
          attachExpr(n.tpe, symbol)
        }
        case _ =>
      }

    }


    def collectFormal(n: Formal, scope: MethodSymbol): Unit = {
      // it's ok if it has the same name as a class variable...
      val formalName = n.id.value
      val symbol = new VariableSymbol(formalName).setPos(n)
      if (scope.lookupVar(formalName).isDefined) {
        error("collectFormal: argList already had a formal with this name!", n)
      } else {
        scope.params += (formalName -> symbol)
        scope.argList = scope.argList :+ symbol
      }
    }


    // ============== Attach ================

    def attachSymbols(node: Tree, scope: GlobalScope): Unit = {
      node match {
        case n: MainMethod => attachMainMethod(n, scope)
        case n: ClassDecl => attachClassDecl(n, scope)
        //      case n: VarDecl => collectVarDecl(n, scope)
        //      case n: MethodDecl => collectMethodDecl(n, scope)
        //      case n: Formal => collectFormal(n, scope)
        //      case n: Identifier => collectIdentifier(n, scope)
        case _ => error("tried to attach " + node + " that needs a more specific scope", node)
      }
    }

    def attachMainMethod(main: MainMethod, scope: GlobalScope): Unit = {
      scope.lookupClass(main.id.value) match {
        case Some(s) => main.setSymbol(s); main.id.setSymbol(s);
        case None => error("attachMainMethod: No symbol for main class", main)
      }
      attachMethod(main.main, main.getSymbol)
    }

    def attachClassDecl(classDecl: ClassDecl, scope: GlobalScope): Unit = {
      val className = classDecl.id.value
      scope.lookupClass(className) match {
        case Some(klass) => classDecl.setSymbol(klass); classDecl.id.setSymbol(klass)
        case None => error("attachClassDecl: No matching class for ID " + className, classDecl)
      }
      if (classDecl.parent.isDefined) {
        val p = classDecl.parent.get
        scope.lookupClass(p.value) match {
          case Some(s) => p.setSymbol(s)
          case None => error("attachClassDecl: parent" + p.value + " had no symbol!!", classDecl)
        }
      }
      unusedClassVars = unusedClassVars ++ classDecl.vars.map(v => v.id.value)
      classDecl.vars.foreach(v => attachVariable(v, classDecl.getSymbol))
      classDecl.methods.foreach(method => attachMethod(method, classDecl.getSymbol))
      if (unusedClassVars.nonEmpty) {
//        warning("Class " + className + " has unused class variables: " + unusedClassVars.toList.mkString(", "), classDecl)
        for (elem <- unusedClassVars) {
          val varSym = classDecl.getSymbol.lookupVar(elem) match {
            case Some(s) => s
            case None => new VariableSymbol("NO MATCHING SYMBOL: " + elem)
          }
          warning("Class " + className + " has unused class variable: " + varSym.name, varSym)
        }
      }
      unusedClassVars = Set()
    }

    def attachMethod(method: MethodDecl, scope: ClassSymbol): Unit = {
      val methodName = method.id.value
      val symbol = scope.lookupMethod(methodName)
      symbol match {
        case Some(z) => method.setSymbol(z); method.id.setSymbol(z)
        case None => error("attachVariable: No matching method " + methodName + " in class scope " + scope.name, method)
      }
      unusedMethodArgs = unusedMethodArgs ++ method.args.map(a => a.id.value)
      unusedMethodVars = unusedMethodVars ++ method.vars.map(v => v.id.value)
      method.args.foreach(formal => attachFormal(formal, method.getSymbol))
      method.vars.foreach(v => attachVariable(v, method.getSymbol))
      (method.exprs :+ method.retExpr).foreach(exp => attachExpr(exp, method.getSymbol))
      attachRetType(method.retType, method.getSymbol)
      if (unusedMethodArgs.nonEmpty) {
        for (elem <- unusedMethodArgs) {
          val argSym = method.getSymbol.lookupVar(elem) match {
            case Some(s) => s
            case None => new VariableSymbol("NO MATCHING SYMBOL: " + elem)
          }
          warning("Method " + scope.name + "." + methodName + " has unused param " + argSym.name, argSym)
        }
      }
      if (unusedMethodVars.nonEmpty) {
        for (elem <- unusedMethodVars) {
          val varSym = method.getSymbol.lookupVar(elem) match {
            case Some(s) => s
            case None => new VariableSymbol("NO MATCHING SYMBOL: " + elem)
          }
          warning("Method " + scope.name + "." + methodName + " has unused var " + varSym.name, varSym)
        }
      }
      unusedMethodArgs = Set()
      unusedMethodVars = Set()
    }

    def attachRetType(tpe: TypeTree, method: MethodSymbol): Unit = {
      attachTypeTree(tpe)
    }

    def attachTypeTree(tpe: TypeTree): Unit = {
      tpe match {
        case tpe: Identifier => {
          // look up in list of classes
          globalScope.lookupClass(tpe.value) match {
            case Some(z) => tpe.setSymbol(z); tpe.setType(TObject(z));
            case None => error("attachTypeTree: No matching class for identifier " + tpe.value, tpe)
          }
        }
        case tpe: IntArrayType => tpe.setType(Types.TIntArray)
        case tpe: IntType => tpe.setType(TInt)
        case tpe: BooleanType => tpe.setType(TBoolean)
        case tpe: StringType => tpe.setType(TString)
        case tpe: UnitType => tpe.setType(TUnit)
        case _ => // do nothing
      }
    }

    def attachFormal(formal: Formal, method: MethodSymbol): Unit = {
      // need to attach the id of the formal AND the type
      attachTypeTree(formal.tpe)
      method.lookupVar(formal.id.value) match {
        case Some(s) => formal.id.setSymbol(s);
        case None => error("attachFormal: no matching symbol for id: " + formal.id.value, formal)
      }
    }

    def attachVariable(v: VarDecl, scope: Symbol): Unit = {

      val varName = v.id.value
      attachTypeTree(v.tpe)
      scope match {
        case s: ClassSymbol => {
          val symbol = s.lookupVar(varName)
          symbol match {
            case Some(z) => v.setSymbol(z); v.id.setSymbol(z);
            case None => error("attachVariable: No matching variable:" + varName + " in  class: " + s.name, v)
          }
        }
        case s: MethodSymbol => {
          val symbol = s.lookupVar(varName)
          symbol match {
            case Some(z) => v.setSymbol(z); v.id.setSymbol(z);
            case None => error("attachVariable: No matching variable: " + varName + " in method: " + s.name, v)
          }
        }
        case _ => {
          error("attachVariable: tried to attach with something that shouldn't have variables", v)
        }
      }
    }





    // main

    // collect symbols
    // should collect all class symbols before symbolzing their methods.
    // add all classes
    prog.classes.foreach(classDecl => addClassSymbol(classDecl, globalScope))
    // for each class, check
    // if class has parent, make sure parent's symbol is there
    prog.classes.foreach(classDecl => checkParent(classDecl, globalScope))
    // then do the for real colleciton on everything else
//    prog.classes.foreach(classDecl => collectClassDecl(classDecl, globalScope))
    // then
    collectMainMethod(prog.main, globalScope)


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // DEPLOY SYMBOLS
    prog.classes.foreach(classDecl => attachSymbols(classDecl, globalScope))
    attachSymbols(prog.main, globalScope)

    prog.classes.foreach(classDecl => println("class " + classDecl.id.value + "#" + classDecl.getSymbol.id))

    // (Step 3:) Print tree with symbol ids for debugging
//    if (ctx.doSymbolIds) {
//      println("doing symbolids 11!!!!")
//      //print tree with symbol ids
//      val out = Printer.applyWithSymbolIds(prog)
//      println(out)
//    }
    // Make sure you check all constraints

    prog
  }



}