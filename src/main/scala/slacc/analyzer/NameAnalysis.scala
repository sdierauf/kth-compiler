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
      collectMethodDecl(n.main, scope.mainClass)
    }

    def collectClassDecl(klass: ClassDecl, scope: GlobalScope): Unit = {
      val className = klass.id.toString
      def addClass(): Unit = {
        val symbol = new ClassSymbol(className)
        scope.classes + (className -> symbol)
        klass.vars.foreach(v => collectVarDecl(v, symbol))
        klass.methods.foreach(m => collectMethodDecl(m , symbol))
      }
      scope.lookupClass(className) match {
        case Some(v) =>
          fatal("collectClassDecl: " + className + " already declared", klass)
        case None =>
          klass.parent match {
            case Some(v: ClassDecl) => {
              if (scope.lookupClass(v.id.toString).isEmpty) {
                fatal("collectClassDecl:" + className + " parent is not defined", klass)
              }
              if (hasInheritanceCycle(klass)) {
                fatal("collectClassDecl: " + className + " has an inheritanceCyckle", klass)
              }
              addClass()
            }
            case None => addClass()
          }
      }
    }

    def hasInheritanceCycle(klass: ClassDecl): Boolean = {
      var visited: Set[ClassDecl] = Set()
      visited = visited + klass
      var currentClass = klass.parent
      while (currentClass.isDefined) {
        currentClass match {
          case Some(k: ClassDecl) => {
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
          s.members + (varName -> symbol)
        }
        case s: MethodSymbol => {
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
      scope.methods + (methodName -> symbol)
      method.args.foreach(arg => collectFormal(arg, symbol))
      method.vars.foreach(v => collectVarDecl(v, symbol))
    }

    def collectFormal(n: Formal, scope: MethodSymbol): Unit = {
      val formalName = n.id.toString
      val symbol = new VariableSymbol(formalName)
      scope.argList = scope.argList :+ symbol
    }

    def collectIdentifier(n: Identifier, scope: Symbol): Unit = {
      scope match {
        case s: ClassSymbol => {
          ???
        }
        case s: MethodSymbol => {
          ???
        }
      }
      ???
    }


    prog.classes.foreach(classdecl => collectSymbols(classdecl, globalScope))
    collectSymbols(prog.main, globalScope)


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging
    if (ctx.doSymbolIds) {
      //print tree with symbol ids
    }
    // Make sure you check all constraints

    prog
  }



}