package slacc.analyzer

import slacc.analyzer.Types._
import slacc.ast.Trees.{ExprTree, Program, _}
import slacc.utils.{Context, Pipeline}

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = ??? // TODO: Compute type for each kind of expression
      expr match {
        case e: And =>
        case e: Or =>
        case e: Plus =>
        case e: Minus =>
        case e: Times =>
        case e: Div =>
        case e: LessThan =>
        case e: Equals =>
        case e: ArrayRead =>
        case e: ArrayLength =>
        case e: MethodCall =>
        case e: IntLit =>
        case e: StringLit =>
        case e: True =>
        case e: False =>
        case e: Identifier =>
        case e: Self =>
        case e: NewIntArray =>
        case e: New =>
        case e: Not =>
        case e: Block =>
        case e: If =>
        case e: While =>
        case e: Println =>
        case e: Assign =>
        case e: ArrayAssign =>
        case e: Strof =>
        case _ => fatal ("tcExpr: what happened")
      }

      def tcAnd(e: And): Unit = {

      }

      def tcOr(e: Or): Unit = {

      }

      def tcPlus(e: Plus): Unit = {

      }

      def tcMinus(e: Minus): Unit = {

      }

      def tcTimes(e: Times): Unit = {

      }

      def tcDiv(e: Div): Unit = {

      }

      def tcLessThan(e: LessThan): Unit = {

      }

      def tcEquals(e: Equals): Unit = {

      }

      def tcArrayRead(e: ArrayRead): Unit = {

      }

      def tcArrayLength(e: ArrayLength): Unit = {

      }

      def tcMethodCall(e: MethodCall): Unit = {

      }

      def tcIntLit(e: IntLit): Unit = {

      }

      def tcStringLit(e: StringLit): Unit = {

      }

      def tcTrue(e: True): Unit = {

      }

      def tcFalse(e: False): Unit = {

      }

      def tcIdentifier(e: Identifier): Unit = {

      }

      def tcSelf(e: Self): Unit = {

      }

      def tcNewIntArray(e: NewIntArray): Unit = {

      }

      def tcNew(e: New): Unit = {

      }

      def tcNot(e: Not): Unit = {

      }

      def tcBlock(e: Block): Unit = {

      }

      def tcIf(e: If): Unit = {

      }

      def tcWhile(e: While): Unit = {

      }

      def tcPrintln(e: Println): Unit = {

      }

      def tcAssign(e: Assign): Unit = {

      }

      def tcArrayAssign(e: ArrayAssign): Unit = {

      }

      def tcStrof(e: Strof): Unit = {

      }

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    prog
  }

}
