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


    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      val classFile = ct.parent match {
        case Some(p) => new ClassFile(ct.id.value, Some(p.value))
        case None => new ClassFile(ct.id.value, None)
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
