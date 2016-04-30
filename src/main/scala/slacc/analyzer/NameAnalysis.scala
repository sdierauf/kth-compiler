package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import slacc.ast.Printer


object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._


    // Step 1: Collect symbols in declarations
    var globalScope = new GlobalScope()

    def addClassSymbols(klass: ClassDecl, scope: GlobalScope): Unit = {
      val className = klass.id.value
      val symbol = new ClassSymbol(className)
      scope.lookupClass(className) match {
        case Some(v) => fatal("collectClassDecl2: already a class with that name defined in the scope");
        case None => scope.classes += (className -> symbol)
          println("addClassSymbols: added " + klass.id.value)
      }
    }

    def checkParent(klass: ClassDecl, scope: GlobalScope): Unit = {
      println("checkParent: looking up " + klass.id.value)
      val symbolOption: Option[ClassSymbol] = scope.lookupClass(klass.id.value)
      val symbol = symbolOption.get
      klass.parent match {
        case Some(p) => {
          val parent = scope.lookupClass(p.value)
          if (parent.isEmpty) {
            fatal("checkParent: no matching symbol for class " + klass.id.value + " parent!!")
          }
          symbol.parent = parent
          // check for inheritance cycle
          if (hasInheritanceCycle(parent.get, scope)) {
            fatal("checkParent: parent of " + klass.id.value + "was part of an inheritance cycle!!")
          }
        }
        case None =>
      }
    }

    // ============ Collect =============
//    def collectSymbols(node: Symbol, scope: GlobalScope): Unit = {
//      node match {
//        case n: MainMethod => collectMainMethod(n, scope)
//        case n: ClassDecl => collectClassDecl(n, scope)
//        //      case n: VarDecl => collectVarDecl(n, scope)
//        //      case n: MethodDecl => collectMethodDecl(n, scope)
//        //      case n: Formal => collectFormal(n, scope)
//        //      case n: Identifier => collectIdentifier(n, scope)
//        case _ => sys.error("tried to collect something that needs to know its symbol scope")
//      }
//    }

    def collectMainMethod(n: MainMethod, scope: GlobalScope): Unit = {
      if (scope.mainClass != null) {
        fatal("collectMainMethod: Main class already declared", n)
      }
      scope.mainClass = new ClassSymbol(n.id.value)
      scope.classes += (n.id.value -> scope.mainClass)
      collectMethodDecl(n.main, scope.mainClass)
    }


    def collectClassDecl(klass: ClassDecl, scope: GlobalScope): Unit = {
      val className = klass.id.value
      val symbol = scope.lookupClass(className).get
      klass.vars.foreach(v => collectVarDecl(v, symbol))
      klass.methods.foreach(m => collectMethodDecl(m , symbol))
    }

    def hasInheritanceCycle(symbol: ClassSymbol, scope: GlobalScope): Boolean = {
      var visited: Set[ClassSymbol] = Set()
      visited = visited + symbol
      var currentClass = symbol.parent
      while (currentClass.isDefined) {
        currentClass match {
          case Some(k) => {
            if (visited.contains(k)) {
              return false
            }
            currentClass = k.parent
          }
          case None => true
        }
      }
      false
    }

    def collectVarDecl(n: VarDecl, scope: Symbol): Unit = {
      val varName = n.id.value
      val symbol = new VariableSymbol(varName)
      scope match {
        case s: ClassSymbol => {
          if (s.lookupVar(varName).isDefined) {
            fatal("collectVarDecl: class " + s.name + " already had a variable named " + varName, n)
          }
          s.members += (varName -> symbol)
        }
        case s: MethodSymbol => {
          // it's ok if has same name as a class variable
          // not ok if it's in declared vars
          if (s.members.get(varName).isDefined || s.params.get(varName).isDefined) {
            fatal("collectVarDecl: method " + s.name + " already had a variable named " + varName, n)
          }
          s.members += (varName -> symbol)

        }
        case _ => {
          sys.error("Collected a variable not in a Class or MethodSymbol!!")
        }
      }
    }

    def collectMethodDecl(method: MethodDecl, scope: ClassSymbol): Unit = {
      val methodName = method.id.value
      val symbol = new MethodSymbol(methodName, scope)
      def addMethod(): Unit = {
        scope.methods += (methodName -> symbol)
        method.args.foreach(arg => collectFormal(arg, symbol))
        method.vars.foreach(v => collectVarDecl(v, symbol))
      }
      // TODO: How the fuck do we know if it's overloaded or not?!?
      scope.lookupMethod(methodName) match {
        case Some(m) => {
          fatal("collectMethodDecl: method " + methodName + " was already defined!", method)
        }
        case None => addMethod()
      }

    }

    def collectFormal(n: Formal, scope: MethodSymbol): Unit = {
      // it's ok if it has the same name as a class variable...
      val formalName = n.id.value
      val symbol = new VariableSymbol(formalName)
      if (scope.argList.contains(symbol)) {
        fatal("collectFormal: argList already had a formal with this name!", n)
      }
      scope.argList = scope.argList :+ symbol
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
        case _ => sys.error("tried to attach something that needs a more specific scope")
      }
    }

    def attachMainMethod(main: MainMethod, scope: GlobalScope): Unit = {
      scope.lookupClass(main.id.value) match {
        case Some(s) => main.setSymbol(s)
        case None => sys.error("attachMainMethod: No symbol for main class")
      }
      attachMethod(main.main, main.getSymbol)
    }

    def attachClassDecl(classDecl: ClassDecl, scope: GlobalScope): Unit = {
      val className = classDecl.id.value
      scope.lookupClass(className) match {
        case Some(klass) => classDecl.setSymbol(klass)
        case None => sys.error("attachClassDecl: No matching class for ID")
      }
      classDecl.methods.foreach(method => attachMethod(method, classDecl.getSymbol))
      classDecl.vars.foreach(v => attachVariable(v, classDecl.getSymbol))
    }

    def attachMethod(method: MethodDecl, scope: ClassSymbol): Unit = {
      val methodName = method.id.value
      val symbol = scope.lookupMethod(methodName)
      symbol match {
        case Some(z) => method.setSymbol(z)
        case None => sys.error("attachVariable: No matching variable in class")
      }
      method.args.foreach(formal => attachFormal(formal, method.getSymbol))
      method.vars.foreach(v => attachVariable(v, method.getSymbol))
      attachRetType(method.retType, method.getSymbol)
    }

    def attachRetType(tpe: TypeTree, method: MethodSymbol): Unit = {
      attachTypeTree(tpe)
    }

    def attachTypeTree(tpe: TypeTree): Unit = {
      tpe match {
        case tpe: Identifier => {
          // look up in list of classes
          globalScope.lookupClass(tpe.value) match {
            case Some(z) => tpe.setSymbol(z)
            case None => sys.error("attachTypeTree: No matching class for identifier")
          }
        }
        case _ => // do nothing
      }
    }

    def attachFormal(formal: Formal, method: MethodSymbol): Unit = {
      // need to attach the id of the formal AND the type
      attachTypeTree(formal.tpe)
      method.lookupVar(formal.id.value) match {
        case Some(s) => formal.id.setSymbol(s)
        case None => sys.error("attachFormal: no matching symbol for id")
      }
    }

    def attachVariable(v: VarDecl, scope: Symbol): Unit = {
      val varName = v.id.value
      scope match {
        case s: ClassSymbol => {
          val symbol = s.lookupVar(varName)
          symbol match {
            case Some(z) => v.setSymbol(z)
            case None => sys.error("attachVariable: No matching variable:" + varName + " in  class: " + s.name )
          }
        }
        case s: MethodSymbol => {
          val symbol = s.lookupVar(varName)
          symbol match {
            case Some(z) => v.setSymbol(z)
            case None => sys.error("attachVariable: No matching variable: " + varName + " in method: " + s.name)
          }
        }
        case _ => {
          sys.error("attachVariable: tried to attach with something that shouldn't have variables")
        }
      }
    }





    // main

    // collect symbols
    // should collect all class symbols before symbolzing their methods.
    // add all classes
    prog.classes.foreach(classDecl => addClassSymbols(classDecl, globalScope))
    // for each class, check
    // if class has parent, make sure parent's symbol is there
    prog.classes.foreach(classDecl => checkParent(classDecl, globalScope))
    // then do the for real colleciton on everything else
    prog.classes.foreach(classDecl => collectClassDecl(classDecl, globalScope))
    // then
    collectMainMethod(prog.main, globalScope)


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // DEPLOY SYMBOLS
    prog.classes.foreach(classDecl => attachSymbols(classDecl, globalScope))
    attachSymbols(prog.main, globalScope)

    // (Step 3:) Print tree with symbol ids for debugging
    if (ctx.doSymbolIds) {
      println("doing symbolids 11!!!!")
      //print tree with symbol ids
      val out = Printer.applyWithSymbolIds(prog)
      println(out)
    }
    // Make sure you check all constraints

    prog
  }



}