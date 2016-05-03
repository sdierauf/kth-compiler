package slacc.analyzer

import slacc.analyzer.Types.{_}
import slacc.ast.Trees.{ExprTree, Program, _}
import slacc.utils.{Context, Pipeline}

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case e: And => tcAnd(e)
        case e: Or => tcOr(e)
        case e: Plus => tcPlus(e)
        case e: Minus => tcMinus(e)
        case e: Times => tcTimes(e)
        case e: Div => tcDiv(e)
        case e: LessThan => tcLessThan(e)
        case e: Equals => tcEquals(e)
        case e: ArrayRead => tcArrayRead(e)
        case e: ArrayLength => tcArrayLength(e)
        case e: MethodCall => tcMethodCall(e)
        case e: IntLit => tcIntLit(e)
        case e: StringLit => tcStringLit(e)
        case e: True => tcTrue(e)
        case e: False => tcFalse(e)
        case e: Identifier => tcIdentifier(e)
        case e: Self => tcSelf(e)
        case e: NewIntArray => tcNewIntArray(e)
        case e: New => tcNew(e)
        case e: Not => tcNot(e)
        case e: Block => tcBlock(e)
        case e: If => tcIf(e)
        case e: While => tcWhile(e)
        case e: Println => tcPrintln(e)
        case e: Assign => tcAssign(e)
        case e: ArrayAssign => tcArrayAssign(e)
        case e: Strof => tcStrof(e)
        case _ => fatal ("tcExpr: what happened")
      }

      def tcAnd(e: And): Type = {
        tcExpr(e.lhs, TBoolean)
        tcExpr(e.rhs, TBoolean)
        TBoolean
      }

      def tcOr(e: Or): Type = {
        tcExpr(e.lhs, TBoolean)
        tcExpr(e.rhs, TBoolean)
        TBoolean
      }

      def tcPlus(e: Plus): Type = {
        val possibleTypes = TInt :: TString :: List()
        val l = tcExpr(e.lhs, possibleTypes:_*)
        val r = tcExpr(e.rhs, possibleTypes:_*)
        l match {
          case TInt => r match {
            case TInt => TInt
            case TString => TString
            case _ => fatal("tcPlus: werent strings or ints")
          }
          case TString => r match {
            case TInt => TString
            case TString => TString
            case _ => fatal("tcPlus: werent strings or ints")
          }
          case _ => fatal("tcPlus: werent strings or ints")
        }
      }

      def tcMinus(e: Minus): Type = {
        val l = tcExpr(e.lhs, TInt)
        val r = tcExpr(e.rhs, TInt)
        TInt
      }

      def tcTimes(e: Times): Type = {
        val l = tcExpr(e.lhs, TInt)
        val r = tcExpr(e.rhs, TInt)
        TInt
      }

      def tcDiv(e: Div): Type = {
        val l = tcExpr(e.lhs, TInt)
        val r = tcExpr(e.rhs, TInt)
        TInt
      }

      def tcLessThan(e: LessThan): Type = {
        val l = tcExpr(e.lhs, TInt)
        val r = tcExpr(e.rhs, TInt)
        TInt
      }

      def tcEquals(e: Equals): Type = {
        val primitives = TInt :: TString :: TBoolean :: TIntArray :: List()
        val accepted: List[Type] = anyObject :: primitives
        val l = tcExpr(e.lhs, accepted:_*)
        l match {
          case TObject(klass) => tcExpr(e.rhs, anyObject); TBoolean
          case p if primitives.contains(l) => tcExpr(e.rhs, p); TBoolean
          case _ => {
            val r : Type = tcExpr(e.rhs)
            fatal("Type error: Equality operator requires compatible types, but found types: "
              + l + " and " + r)
            TError
          }
        }
      }

      def tcArrayRead(e: ArrayRead): Type = {

      }

      def tcArrayLength(e: ArrayLength): Type = {

      }

      def tcMethodCall(e: MethodCall): Type = {

      }

      def tcIntLit(e: IntLit): Type = {

      }

      def tcStringLit(e: StringLit): Type = {

      }

      def tcTrue(e: True): Type = {

      }

      def tcFalse(e: False): Type = {

      }

      def tcIdentifier(e: Identifier): Type = {

      }

      def tcSelf(e: Self): Type = {

      }

      def tcNewIntArray(e: NewIntArray): Type = {

      }

      def tcNew(e: New): Type = {

      }

      def tcNot(e: Not): Type = {

      }

      def tcBlock(e: Block): Type = {

      }

      def tcIf(e: If): Type = {

      }

      def tcWhile(e: While): Type = {

      }

      def tcPrintln(e: Println): Type = {

      }

      def tcAssign(e: Assign): Type = {

      }

      def tcArrayAssign(e: ArrayAssign): Type = {

      }

      def tcStrof(e: Strof): Type = {

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
