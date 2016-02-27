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
    val buf = new StringBuilder()
    t.classes.foreach(klass => buf.append(apply(klass)))
    buf.append(apply(t.main))
    buf.toString()
  }

  def printMainMethod(t: MainMethod): String = {
    val buf = new StringBuilder()
    buf.append("method Main {\n")
    t.exprs.foreach(exp => buf.append(apply(exp)))
    buf.append("}\n")
    buf.toString()
  }

  def printClassDecl(t: ClassDecl): String = {
    val buf = new StringBuilder()
    buf.append("class " + apply(t.id))
    t.parent match {
      case Some(parent: Identifier) => buf.append(" <: " + apply(parent) + " ")
      case _ =>
    }
    buf.append("{\n")
    t.vars.foreach(v => buf.append(apply(v)))
    t.methods.foreach(m => buf.append(apply(m)))
    buf.append("}\n")
    buf.toString()
  }

  def printVarDecl(t: VarDecl): String = {
    val buf = new StringBuilder()
    buf.append("var ")
      .append(apply(t.id))
      .append(" : ")
      .append(apply(t.tpe))
      .append("\n")
    buf.toString()
  }

  def printMethodDecl(t: MethodDecl): String =  {
    val buf = new StringBuilder()
    buf.append("method ").append(apply(t.id)).append(" (")
    val formalStrings: List[String] = t.args.map(formal => apply(formal))
    buf.append(formalStrings.mkString(","))
    buf.append(") ").append(apply(t.retType)).append(" = { ")
    t.vars.foreach(v => buf.append(apply(v)))
    val exprString: List[String] = (t.exprs :+ t.retExpr).map(exp => apply(exp))
    buf.append(exprString.mkString(";\n")).append("}\n")
    buf.toString()
  }

  def printFormal(t: Formal): String = {
    val buf = new StringBuilder()
    buf.append(apply(t.id)).append(" : ").append(apply(t.tpe))
    buf.toString()
  }

  def printType(t: TypeTree): String = {
    t match {
      case _ : IntArrayType => "Int[]"
      case _ : IntType => "Int"
      case _ : BooleanType => "Bool"
      case _ : StringType => "String"
      case _ : UnitType => "Unit"
    }
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
    ???
  }

  def printOr(t: Or): String ={
    ???
  }

  def printPlus(t: Plus): String = {
    ???
  }

  def printMinus(t: Minus): String ={
    ???
  }

  def printTimes(t: Times): String ={
    ???
  }

  def printDiv(t: Div): String = {
    ???
  }

  def printLessThan(t: LessThan): String = {
    ???
  }

  def printEquals(t: Equals): String ={
    ???
  }

  def printArrayRead(t: ArrayRead): String ={
    ???
  }

  def printArrayLength(t: ArrayLength): String ={
    ???
  }

  def printMethodCall(t: MethodCall): String = {
    ???
  }

  def printIntLit(t: IntLit): String ={
    ???
  }

  def printStringLit(t: StringLit): String ={
    ???
  }

  def printTrue(t: True): String = {
    ???
  }

  def printFalse(t: False): String = {
    ???
  }

  def printIdentifier(t: Identifier): String = {
    ???
  }

  def printSelf(t: Self): String = {
    ???
  }

  def printNewIntArray(t: NewIntArray): String = {
    ???
  }

  def printNew(t: New): String = {
    ???
  }

  def printNot(t: Not): String = {
    ???
  }

  def printBlock(t: Block): String = {
    ???
  }

  def printIf(t: If): String = {
    ???
  }

  def printWhile(t: While): String = {
    ???
  }

  def printPrintln(t: Println): String = {
    ???
  }

  def printAssign(t: Assign): String = {
    ???
  }

  def printArrayAssign(t: ArrayAssign): String = {
    ???
  }

  def printStrof(t: Strof): String = {
    ???
  }

}
