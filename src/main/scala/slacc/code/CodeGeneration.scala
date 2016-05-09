package slacc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    def addFieldToClass(cls:ClassFile, name: String, tpe:Type): Unit = {
      cls.addField(getPrefixForType(tpe), name)
    }

    def addMethodToClass(cls: ClassFile, name: String, args : List[VariableSymbol], returnType: Type): CodeHandler = {
      var paramsString = new StringBuilder()
      val paramsList : List[Type] = List()
      args.foreach(z => paramsList:+(z.getType))
      paramsList.foreach(p => paramsString.append(getPrefixForType(p)))
      cls.addMethod(getPrefixForType(returnType), name, paramsString.toString).codeHandler
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      val classFile = ct.parent match {
        case Some(p) => new ClassFile(ct.id.value, Some(p.value))
        case None => new ClassFile(ct.id.value, None)
      }
      // Add fields
      ct.vars.foreach(v => addFieldToClass(classFile, v.id.value, v.getSymbol.getType))
      // Add methods
      for (m <- ct.methods) {
        val ch = addMethodToClass(classFile, m.id.value, m.getSymbol.argList, m.retType.getType)
        generateMethodCode(ch, m)
      }
      classFile.setSourceFile(sourceName)
      val fileDest = dir match {
        case "" => "./"
        case _ => dir
      }
      classFile.writeToFile(fileDest + ct.id.value + ".class")
    }

    def getPrefixForType(typ: Type): String = {
      typ match {
        case TError => fatal("getPrefixForType: got " + TError)
        case TUntyped => fatal("getPrefixForType: got " + TUntyped)
        case TInt => "I"
        case TBoolean => "Z"
        case TString => "L"
        case TUnit => "V" //void
        case TIntArray => "[I"
        case anyObject => "?"
        case _ => fatal("getPrefixForType: got " + typ)
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // TODO: Emit code
      def expr2Code (ex: ExprTree): Unit = {
        ex match {
            // in each case cons onto ch
            // ch << blah << blah << blah
          case t : And => {
            expr2Code(t.lhs)
            expr2Code(t.rhs)
          } case t : Or => {
            expr2Code(t.lhs)
            expr2Code(t.rhs)
          } case t : Plus => {
            if (t.getType == TInt) {
              // Addition
            } else {
              // String concat
            }
          } case t : Minus => {
            expr2Code(t.lhs)
            expr2Code(t.rhs)
          } case t : Times => {
            expr2Code(t.lhs)
            expr2Code(t.rhs)
          } case t : Div => {
            expr2Code(t.lhs)
            expr2Code(t.rhs)
          } case t : LessThan => {
            expr2Code(t.lhs)
            expr2Code(t.rhs)
          } case t : Equals => {
            if (t.lhs.getType == TBoolean) {

            } else if (t.lhs.getType == TInt) {

            } else {
              // String and object reference comparisons
            }
          } case b : Block => {
            b.exprs.foreach(e => expr2Code(e))
          } case ifthen : If => {
            expr2Code(ifthen.expr)
            expr2Code(ifthen.thn)
            ifthen.els match {
              case Some(e3) => expr2Code(e3)
              case None =>
            }
          } case w : While => {
            expr2Code(w.body)
            expr2Code(w.cond)
          } case p : Println => {
            expr2Code(p.expr)
          } case s : Strof => {
            if (s.expr.getType != TString) {

            }
          }
          case a : Assign => {
            expr2Code(a.id)
            expr2Code(a.expr)
          } 
          case i : Identifier => {
          }
          case m :MethodCall => {
            expr2Code(m.obj)
            expr2Code(m.meth)
            m.args.foreach(ar => expr2Code(ar))
          }
          case e: ArrayRead => {
            expr2Code(e.arr)
            expr2Code(e.index)
          }
          case e: ArrayLength => {
            expr2Code(e.arr)
          }
          case e: ArrayAssign => {
            expr2Code(e.id)
            expr2Code(e.expr)
            expr2Code(e.index)
          }
          case n: New => {
            attachTypeTree(n.tpe)
            expr2Code(n.tpe)
          }
        }
      }

      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.files.head.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main method
    // ...
  }

}
