package slacc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    t match {
      case t: Program => printProgram(t)
      case t: MainMethod => printMainMethod(t)
      case t: ClassDecl => printClassDecl(t)
      case t: VarDecl => printVarDecl(t)
      case t: MethodDecl => printMethodDecl(t)
      case t: Formal => printFormal(t)
      case t: TypeTree => printType(t)
      case t: ExprTree => printExpression(t)
    }
  }

  def printProgram(t: Program): String = {
    ???
  }

  def printMainMethod(t: MainMethod): String = {
    ???
  }

  def printClassDecl(t: ClassDecl): String = {
    ???
  }

  def printVarDecl(t: VarDecl): String = {
    ???
  }

  def printMethodDecl(t: MethodDecl): String =  {
    ???
  }

  def printFormal(t: Formal): String = {
    ???
  }

  def printType(t: TypeTree): String = {
    ???
  }

  def printExpression(t: ExprTree): String = {
    t match {
      case t: And => printAnd(t)
      case t: Or => printOr(t)
      case t: Plus => printPlus(t)
      case t: Minus => printMinus(t)
      case t: Times => printTimes(t)
      case t: Div => printDiv(t)
      case t: LessThan => printLessThan(t)
      case t: Equals => printEquals(t)
      case t: ArrayRead => printArrayRead(t)
      case t: ArrayLength => printArrayLength(t)
      case t: MethodCall => printMethodCall(t)
      case t: IntLit => printIntLit(t)
      case t: StringLit => printStringLit(t)
      case t: True => printTrue(t)
      case t: False => printFalse(t)
      case t: Identifier => printIdentifier(t)
      case t: Self => printSelf(t)
      case t: NewIntArray => printNewIntArray(t)
      case t: Not => printNot(t)
      case t: Block => printBlock(t)
      case t: If => printIf(t)
      case t: While => printWhile(t)
      case t: Println => printPrintln(t)
      case t: Assign => printAssign(t)
      case t: ArrayAssign => printArrayAssign(t)
      case t: Strof => printStrof(t)
    }
  }

  def printAnd(t: And): String = {
    apply(t.lhs) + " && " + apply(t.rhs)
  }

  def printOr(t: Or): String ={
    apply(t.lhs) + " || " + apply(t.rhs)
  }

  def printPlus(t: Plus): String = {
    apply(t.lhs) + " + " + apply(t.rhs)
  }

  def printMinus(t: Minus): String ={
    apply(t.lhs) + " - " + apply(t.rhs)
  }

  def printTimes(t: Times): String ={
    apply(t.lhs) + " * " + apply(t.rhs)
  }

  def printDiv(t: Div): String = {
    apply(t.lhs) + " / " + apply(t.rhs)
  }

  def printLessThan(t: LessThan): String = {
    apply(t.lhs) + " < " + apply(t.rhs)
  }

  def printEquals(t: Equals): String ={
    apply(t.lhs) + " == " + apply(t.rhs)
  }

  def printArrayRead(t: ArrayRead): String ={
    apply(t.arr) + "[" + apply(t.index) + "]" 
  }

  def printArrayLength(t: ArrayLength): String ={
    apply(t.arr) + ".length"
  }

  def printMethodCall(t: MethodCall): String = {
    var s = new StringBuilder(apply(t.obj) + " " + apply(t.meth) +"(")
    val arg : List[String] = t.args.map(e => apply(e))
    s ++= expressions.mkString(",")  
    (s ++= ")").toString
  }

  def printIntLit(t: IntLit): String ={
    t.value.toString
  }

  def printStringLit(t: StringLit): String ={
    t.value
  }

  def printTrue(t: True): String = {
    "true"
  }

  def printFalse(t: False): String = {
    "false"
  }

  def printIdentifier(t: Identifier): String = {
    t.value
  }

  def printSelf(t: Self): String = {
    "self"
  }

  def printNewIntArray(t: NewIntArray): String = {
    "new " + "Int[" + apply(t.size) "]"
  }

  def printNew(t: New): String = {
    "new " + apply(t.tpe)
  }

  def printNot(t: Not): String = {
    "!" + apply(t.expr)
  }

  def printBlock(t: Block): String = {
    var s = new StringBuilder("{")
    val expressions : List[String] = t.exprs.map(e => apply(e))
    s ++= expressions.mkString(";\n") // thank you brilliant stefan 
    (s ++= "}").toString
  }

  def printIf(t: If): String = t.els match {
    case Some(els) => {
      elseBody = "else " + printExpression(ex)
    } case None => {
      elseBody = ""
    }
    "if (" + apply(t.expr) + ")" + apply(thn) + elseBody
  }

  def printWhile(t: While): String = {
    "while (" + apply(t.cond) + ") " + apply(t.body)
  }

  def printPrintln(t: Println): String = {
    "println(" + apply(t.expr) +")"
  }

  def printAssign(t: Assign): String = {
    apply(t.id) + " = " + apply(t.expr)
  }

  def printArrayAssign(t: ArrayAssign): String = {
    apply(t.id) + "[" + apply(t.index) + "] =" + apply(t.expr) 
  }

  def printStrof(t: Strof): String = {
     "srtOf(" + apply(t.expr) + ")"
  }

}
