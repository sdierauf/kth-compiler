package slacc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

import scala.collection.mutable

object CodeGeneration extends Pipeline[Program, Unit] {

  var slot : mutable.Map[VariableSymbol, Integer] = mutable.Map()

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    def addFieldToClass(cls:ClassFile, name: String, tpe:Type): Unit = {
      cls.addField(getPrefixForType(tpe), name)
    }


    def addMethodToClass(cls: ClassFile, name: String, sym: MethodSymbol, returnType: Type): CodeHandler = {
      var paramsString = new mutable.StringBuilder()
      val paramsList : mutable.ListBuffer[Type] = mutable.ListBuffer()
      for (k <- sym.argList) {
        paramsList += k.getType
      }
      paramsList.foreach(p => paramsString.append(getPrefixForType(p)))
      cls.addMethod(getPrefixForType(returnType), name, paramsString.toString).codeHandler
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {

      val classFile = ct.parent match {
        case Some(p) => new ClassFile(ct.id.value, Some(p.value))
        case None => new ClassFile(ct.id.value, None)
      }
      // Add fields
      ct.vars.foreach(v => addFieldToClass(classFile, v.id.value, v.tpe.getType))

      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      // Add methods
      for (m <- ct.methods) {
        val ch = addMethodToClass(classFile, m.id.value, m.getSymbol, m.retType.getType)
        generateMethodCode(ch, m)
      }

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
        case TString => "Ljava/lang/String;"
        case TUnit => "V" //void
        case TIntArray => "[I"
        case TObject(c) => "L" + c.name+ ";"
        case _ => fatal("getPrefixForType: got " + typ)
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      slot.empty // clear out variable associations
      mt.vars.foreach(v => slot(v.getSymbol) = ch.getFreshVar)
      ch << Comment("Loading method " + mt.id.value + " in class " + mt.getSymbol.classSymbol.name)
      def generateExprCode (ex: ExprTree): Unit = {
        ex match {
          case t : And => {
            val labelName = ch.getFreshLabel("shortCircuitAnd")
            val endLabel = ch.getFreshLabel("endLabel")
            generateExprCode(t.lhs) // push lhs on stack
            ch << Ldc(0) // push 0 on the stack
            ch << If_ICmpEq(labelName) // if LHS is False (== 0), jump to the labelName, don't eval rhs
            generateExprCode(t.rhs) // whatever is pushed on the stack here is the result of the eval
            ch << Ldc(0)
            ch << If_ICmpEq(labelName) // get here, LHS holds, and now cmp RHS
            ch << Ldc(1) // both expr are true
            ch << Goto(endLabel)
            ch << Label(labelName) // jumps here if !LHS
            ch << Ldc(0)
            ch << Label(endLabel)
          } case t : Or => {
            val labelName = ch.getFreshLabel("shortCircuitOr")
            val endLabel = ch.getFreshLabel("endLabel")
            generateExprCode(t.lhs) // push lhs on stack
            ch << Ldc(1) // opposite of case above
            ch << If_ICmpEq(labelName)
            generateExprCode(t.rhs)
            ch << Ldc(1)
            ch << If_ICmpEq(labelName) // get here LHS does not hold, need to check RHS
            ch << Ldc(0) // neither are true
            ch << Goto(endLabel)
            ch << Label(labelName)
            ch << Ldc(1)
            ch << Label(endLabel)
          } case t : Plus => {
            if (t.getType == TInt) {
              // Addition - trick is to load left and right hand sides first... i think
              generateExprCode(t.lhs)
              generateExprCode(t.rhs)
              ch << IADD
            } else if (t.getType == TString) {
              ch << DefaultNew("java/lang/StringBuilder")
              generateExprCode(t.lhs)
              if (t.lhs.getType == TString) {
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              } else { // its an int
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              }
              generateExprCode(t.rhs)
              if (t.rhs.getType == TString) {
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              } else { // its an int
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              }
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            }
          } case t : Minus => {
            generateExprCode(t.lhs)
            generateExprCode(t.rhs)
            ch << ISUB
          } case t : Times => {
            generateExprCode(t.lhs)
            generateExprCode(t.rhs)
            ch << IMUL
          } case t : Div => {
            generateExprCode(t.lhs)
            generateExprCode(t.rhs)
            ch << IDIV
          } case t : LessThan => {
            val labelName = ch.getFreshLabel("GEQ")
            val endLabel = ch.getFreshLabel("endLabel")
            generateExprCode(t.lhs)
            generateExprCode(t.rhs)
            ch << If_ICmpGe(labelName) // if LHS >= RHS jump to label
            ch << Ldc(1) // if here it's true LHS < RHS
            ch << Goto(endLabel)
            ch << Label(labelName)
            ch << Ldc(0)
            ch << Label(endLabel)
          } case t : Equals => {
            val labelName = ch.getFreshLabel("notEqual")
            val endLabel = ch.getFreshLabel("endLabel")
            generateExprCode(t.lhs) // counting on true to always push 1 to the stack :/
            generateExprCode(t.rhs)
            ch << If_ICmpNe(labelName)
            ch << Ldc(1)
            ch << Goto(endLabel)
            ch << Label(labelName)
            ch << Ldc(0)
            ch << Label(endLabel)
          } case b : Block => {
            for (e <- b.exprs) {
              generateExprCode(e)
              e.getType match {
                case TUnit => {}
                case _ => ch << POP
              }
            }
            //b.exprs.foreach(e => generateExprCode(e))
          } case ifthen : If => {
            val thn = ch.getFreshLabel("thenBranch")
            val els = ch.getFreshLabel("elseBranch")
            val exit = ch.getFreshLabel("exitBranch")
            generateExprCode(ifthen.expr)
            ch << IfEq(if (ifthen.els.isDefined) els else exit)
            generateExprCode(ifthen.thn)
            ch << Goto(exit)
            if (ifthen.els.isDefined) {
              ch << Label(els)
              generateExprCode(ifthen.els.get)
            }
            ch << Label(exit)
          } case w : While => {
            val labelName = ch.getFreshLabel("continue")
            val labelNameQuit = ch.getFreshLabel("quit")
            ch << Label(labelName)
            generateExprCode(w.cond)
            ch << Ldc(0)
            ch << If_ICmpEq(labelNameQuit) // see if the condition is false, jump to quit
            generateExprCode(w.body)
            ch << Goto(labelName) // do the body and go back to continue label
            ch << Label(labelNameQuit)
          } case p : Println => {
            ch << Comment("Printing " + p.expr)
            ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
            generateExprCode(p.expr)
            ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
          } case s : Strof => {
            s.expr.getType match {
              case TInt => {
                ch << DefaultNew("java/lang/StringBuilder")
                generateExprCode(s.expr) // load arg onto stack
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
                ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
              }
              case TBoolean => {
                ch << DefaultNew("java/lang/StringBuilder")
                generateExprCode(s.expr) // load arg onto stack
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Z)Ljava/lang/StringBuilder;")
                ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
              }
              case _ => {

              }
            }
          }
          case a : Assign => {
            // look up the symbol
            val mvSym = methSym.lookupUnique(a.id.value)
            val cfSym = methSym.classSymbol.lookupVar(a.id.value)

            if (cfSym.isDefined && !mvSym.isDefined) {
              ch << Comment("Assigning class field " + a.id.value + " in " + methSym.classSymbol.name +
                " with type " + getPrefixForType(a.id.getType))
              ch << ArgLoad(0) //  gotta get class on the stack
              generateExprCode(a.expr)
              ch << PutField(methSym.classSymbol.name, a.id.value, getPrefixForType(a.id.getType))
            } else if (mvSym.isDefined) {
              val s = mvSym.get
              if (methSym.argList.contains(s)) {
                val n = methSym.argList.indexOf(s)
                ch << Comment("Assigning " + s.name)
                generateExprCode(a.expr)
                // ch << ArgLoad(n + 1) // +1 since 0 refers to "this"
                s.getType match {
                  case TBoolean => {
                    ch << IStore(n + 1)
                  }
                  case TInt => {
                    ch << IStore(n + 1)
                  }
                  case _ => {
                    // it a reference
                    ch << AStore(n + 1)
                  }
                }
              } else {
                if (!slot.contains(s)) {
                  // may not be neccessary
                  ch << Comment("Adding slot for " + a.id.value + " with symbol " + s)
                  slot(s) = ch.getFreshVar
                }
                val n = slot(s)
                generateExprCode(a.expr) // put it on the stack
                s.getType match {
                  case TBoolean => {
                    ch << IStore(n)
                  }
                  case TInt => {
                    ch << IStore(n)
                  }
                  case _ => {
                    // it a reference
                    ch << Comment("Storing reference at " + n)
                    ch << AStore(n)
                  }
                }
              }
            } else {
              error("Look up failed for " + a.id.value)
            }
          } 
          case i : Identifier => { // what if it's an Arg?
            val mvSym = methSym.lookupUnique(i.value)
            val cfSym = methSym.classSymbol.lookupVar(i.value)

            if (cfSym.isDefined && !mvSym.isDefined) {
              ch << Comment("Getting field " + i.value)
              ch << ArgLoad(0) // get class on stack
              ch << GetField(methSym.classSymbol.name, i.value, getPrefixForType(i.getType))
            } else if (mvSym.isDefined) {
              val s = mvSym.get
              if (methSym.argList.contains(s)) {
                val n = methSym.argList.indexOf(s)
                ch << ArgLoad(n + 1) // +1 since 0 refers to "this"
              } else {
                ch << Comment("Loading " + i.value)
                val n = slot(s) // get where it's stored
                s.getType match {
                  case TBoolean => {
                    ch << ILoad(n) // do i remove it from the mapping? what if i need it again?
                  }
                  case TInt => {
                    ch << ILoad(n)
                  }
                  case _ => {
                    ch << ALoad(n) // this ok for arrays
                  }
                }
              }
            } else {
              error("Look up failed for " + i.value)
            }
            }

          case m :MethodCall => {
            val acc = new StringBuilder()
            generateExprCode(m.obj) // push receive onto the stack - think this is the right order
            m.args.foreach(a => generateExprCode(a)) // push args onto the stack
            m.meth.getSymbol.asInstanceOf[MethodSymbol].argList.foreach(a => acc.append(getPrefixForType(a.getType)))
            val methSig = "(" + acc.toString + ")" + getPrefixForType(m.meth.getSymbol.getType)
            ch << Comment("Calling method " + m.meth.value)
            ch << Comment("With method signature " + methSig + " on object " + m.obj.getType.toString)
            ch << InvokeVirtual(m.obj.getType.toString, m.meth.value, methSig)
          }
          case e: ArrayRead => {
            generateExprCode(e.arr)
            generateExprCode(e.index)
            ch << IALOAD
          }
          case e: ArrayLength => {
            generateExprCode(e.arr)
            ch << ARRAYLENGTH
          }
          case e: ArrayAssign => {
            generateExprCode(e.id)
            generateExprCode(e.index)
            generateExprCode(e.expr) // integer to be stored
            ch << IASTORE
          }
          case n: New => {
            ch << Comment("New " + n.tpe.value)
            ch << DefaultNew(n.tpe.value)
          }
          case s: Self => {
            ch << ArgLoad(0)
          }
          case n: Not => {
            val labelName = ch.getFreshLabel("not")
            val endLabel = ch.getFreshLabel("endLabel")
            generateExprCode(n.expr)
            ch << Ldc(0)
            ch << If_ICmpEq(labelName)
            ch << Ldc(0)
            ch << Goto(endLabel)
            ch << Label(labelName)
            ch << Ldc(1)
            ch << Label(endLabel)
          }
          case i : NewIntArray => {
            generateExprCode(i.size) // push size to onto the stack
            ch << NewArray(10) // 10 is T_INT - creates a new int array and pushes it on the stack - but where to store it
          }
          case i : IntLit => {
            ch << Ldc(i.value)
          }
          case b1 : True => {
            ch << Ldc(1)
          }
          case b2 : False => {
            ch << Ldc(0)
          }
          case s : StringLit => {
            ch << Comment(s.value + " pushed onto the stack")
            ch << Ldc(s.value)
          }
        }

      }

      // mt.vars.foreach(e => generateExprCode(e))
      for (e <- mt.exprs) {
        generateExprCode(e)
        e.getType match {
          case TUnit => {};
          case _ => ch << POP;
        }
      }

      generateExprCode(mt.retExpr)
      mt.retType.getType match {
        case TInt => ch << IRETURN
        case TUnit => ch << RETURN
        case TBoolean => ch << IRETURN
        case TObject(cs) => ch << ARETURN // return a reference
        case TString => ch << ARETURN
        case TIntArray => ch << ARETURN
        case _ =>
      }

      ch.print
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

    val mainClass = new ClassFile("Main", None)
    // Now do the main method
    mainClass.addDefaultConstructor
    val mainHandler = mainClass.addMainMethod.codeHandler
    generateMethodCode(mainHandler, prog.main.main)
    mainClass.writeToFile("Main.class")
  }

}
