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

  def printProgram(t: Program): Unit = {
    ???
  }

  def printMainMethod(t: MainMethod) = {
    ???
  }

  def printClassDecl(t: ClassDecl) = {
    ???
  }

  def printVarDecl(t: VarDecl) = {
    ???
  }

  def printMethodDecl(t: MethodDecl) =  {
    ???
  }

  def printFormal(t: Formal) = {
    ???
  }

  def printType(t: TypeTree) = {
    ???
  }

  def printExpression(t: ExprTree) = {
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

  def printAnd(t: And): Unit = {
    ???
  }

  def printOr(t: Or): Unit ={
    ???
  }

  def printPlus(t: Plus): Unit ={

  }

  def printMinus(t: Minus): Unit ={

  }

  def printTimes(t: Times): Unit ={

  }

  def printDiv(t: Div): Unit = {

  }

  def printLessThan(t: LessThan): Unit = {

  }

  def printEquals(t: Equals): Unit ={

  }

  def printArrayRead(t: ArrayRead): Unit ={

  }

  def printArrayLength(t: ArrayLength): Unit ={

  }

  def printMethodCall(t: MethodCall): Unit = {

  }

  def printIntLit(t: IntLit): Unit ={

  }

  def printStringLit(t: StringLit): Unit ={

  }

  def printTrue(t: True): Unit = {

  }

  def printFalse(t: False): Unit = {

  }

  def printIdentifier(t: Identifier): Unit = {

  }

  def printSelf(t: Self): Unit = {

  }

  def printNewIntArray(t: NewIntArray): Unit = {

  }

  def printNew(t: New): Unit = {

  }

  def printNot(t: Not): Unit = {

  }

  def printBlock(t: Block): Unit = {

  }

  def printIf(t: If): Unit = {

  }

  def printWhile(t: While): Unit = {

  }

  def printPrintln(t: Println): Unit = {

  }

  def printAssign(t: Assign): Unit = {

  }

  def printArrayAssign(t: ArrayAssign): Unit = {

  }

   def printStrof(t: Strof): Unit = {

   }
  
}
