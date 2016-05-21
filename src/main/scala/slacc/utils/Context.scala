package slacc
package utils

import java.io.File

case class Context(
  val reporter: Reporter,
  val files: List[File] = Nil,
  val outDir: Option[File] = None,
  val doEval: Boolean = false,
  val doHelp: Boolean = false,
  val doPrintMain: Boolean = false,
  val doTokens: Boolean = false,
  val doAST: Boolean = false,
  val testPrint: Boolean = false,
  val testAst: Boolean = false,
  val testParse: Boolean = false,
  val testPrintAll: Boolean = false,
  val doSymbolIds: Boolean = false,
  val doRepl: Boolean = false
)
