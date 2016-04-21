package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._


object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._


    // Step 1: Collect symbols in declarations
    var globalScope = new GlobalScope()

    def collectSymbols(node: Symbolic, scope: GlobalScope): Unit = {
      node match {
        case n: MainMethod => collectMainMethod(n, scope)
        case n: ClassDecl => collectClassDecl(n, scope)
        //      case n: VarDecl => collectVarDecl(n, scope)
        //      case n: MethodDecl => collectMethodDecl(n, scope)
        //      case n: Formal => collectFormal(n, scope)
        //      case n: Identifier => collectIdentifier(n, scope)
        case n: _ => sys.error("tried to collect something that needs to know its symbol scope")
      }
    }

    def collectMainMethod(n: MainMethod, scope: GlobalScope): Unit = {
      if (scope.mainClass != null) {
        fatal("collectMainMethod: Main class already declared", n)
      }
      scope.mainClass = new ClassSymbol(n.id.toString)
      n.setSymbol(scope.mainClass)
      collectMethodDecl(n.main, scope.mainClass)
    }

    def collectClassDecl(klass: ClassDecl, scope: GlobalScope): Unit = {
      val className = klass.id.toString
      val symbol = new ClassSymbol(className)
      klass.setSymbol(symbol)
      def addClass(): Unit = {
        scope.classes + (className -> symbol)
        klass.vars.foreach(v => collectVarDecl(v, symbol))
        klass.methods.foreach(m => collectMethodDecl(m , symbol))
      }
      scope.lookupClass(className) match {
        case Some(v) =>
          fatal("collectClassDecl: " + className + " already declared", klass)
        case None =>
          klass.parent match {
            case Some(v) => {
              if (scope.lookupClass(v.value).isEmpty) {
                fatal("collectClassDecl:" + className + " parent is not defined", klass)
              }
              symbol.parent = scope.lookupClass(v.value)
              if (hasInheritanceCycle(symbol, scope)) {
                fatal("collectClassDecl: " + className + " has an inheritanceCyckle", klass)
              }
              addClass()
            }
            case None => addClass()
          }
      }
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
      val varName = n.id.toString
      val symbol = new VariableSymbol(varName)
      scope match {
        case s: ClassSymbol => {
          if (s.lookupVar(varName).isDefined) {
            fatal("collectVarDecl: class " + s.name + " already had a variable named " + varName, n)
          }
          s.members + (varName -> symbol)
        }
        case s: MethodSymbol => {
          // it's ok if has same name as a class variable
          // not ok if it's in declared vars
          if (s.lookupVar(varName).isDefined) {
            fatal("collectVarDecl: method " + s.name + " already had a variable named " + varName, n)
          }
          s.members + (varName -> symbol)

        }
        case s: _ => {
          sys.error("Collected a variable not in a Class or MethodSymbol!!")
        }
      }
    }

    def collectMethodDecl(method: MethodDecl, scope: ClassSymbol): Unit = {
      val methodName = method.id.toString
      val symbol = new MethodSymbol(methodName, scope)
      def addMethod(): Unit = {
        scope.methods + (methodName -> symbol)
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
      val formalName = n.id.toString
      val symbol = new VariableSymbol(formalName)
      if (scope.argList.contains(symbol)) {
        fatal("collectFormal: argList already had a formal with this name!", n)
      }
      scope.argList = scope.argList :+ symbol
    }


    prog.classes.foreach(classdecl => collectSymbols(classdecl, globalScope))
    collectSymbols(prog.main, globalScope)


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
//    var p = new Identifier("poop")
    // TODO: what the fuck does attach mean??1?????????

    // (Step 3:) Print tree with symbol ids for debugging
    if (ctx.doSymbolIds) {
      //print tree with symbol ids

    }
    // Make sure you check all constraints

    prog
  }



}