package slacc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  class EndOfInput(message: String) extends RuntimeException(message) {}

  val simpleTokens = Map((':', COLON), ('(', LPAREN), (')', RPAREN), (';', SEMICOLON), ('.', DOT),
    (',', COMMA), ('!', BANG), ('{', LBRACE), ('[', LBRACKET), ('}', RBRACE), (']', RBRACKET),
    ('<', LESSTHAN), ('+', PLUS), ('-', MINUS), ('*', TIMES))

  val keywords = Map(("class", CLASS), ("true", TRUE), ("false", FALSE), ("var", VAR), ("unit", UNIT),
    ("string", STRING), ("extends", EXTENDS), ("int", INT), ("boolean", BOOLEAN), ("while", WHILE),
    ("if", IF), ("else", ELSE), ("length", LENGTH), ("self", SELF), ("new", NEW), ("println", PRINTLN),
    ("strOf", STROF))

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f).getLines.mkString
    var pos = 0
    var eof: Boolean = false

    import ctx.reporter._

    new Iterator[Token] {

      def hasNext : Boolean = {
        !eof
      }

      def next : Token = {

        if (eof) throw new EndOfInput("reading" + f)

        if (pos > source.length - 1){ // check if end of file
          eof = true
          new Token(EOF).setPos(f, pos)
        }

        var c = source.charAt(pos)

        // Whitespace
        if (c.isSpaceChar) {
          var isWhiteSpace = true
          while (pos < source.length - 1 && isWhiteSpace){ // skip white space
            pos += 1
            c = source.charAt(pos)
            if (!c.isSpaceChar){
              isWhiteSpace = false
            }
          }
          if (isWhiteSpace) { // we've reached the end of the file
            eof = true
            new Token(EOF).setPos(f, pos)
          }

        }

        // Comments
        if (c.equals('/')){
          pos += 1
          c = source.charAt(pos)
          if (c.equals('/')) {
            while (pos < source.length -1 && !c.equals('\n')) {
              // single line comment case
              pos += 1
              c = source.charAt(pos)
            }
          } else if (c.equals('*')) {
            var finished = false
            while (pos < source.length && !finished) {
              pos += 1
              c = source.charAt(pos)
              if (c.equals('*') && pos < source.length - 1){ // have to look ahead
                pos += 1
                c = source.charAt(pos)
                if (c.equals('/')){
                  finished = true
                }
              }
            }
          } else {
            new Token(DIV).setPos(f, pos)
          }
        }

        // simple punctuation type stuff
        if (simpleTokens.contains(c)){
          pos += 1
          new Token(simpleTokens(c)).setPos(f, pos)
        }

        // Equals or assignment
        // cases where we have to look ahead
        if (c.equals('=') && pos < source.length() - 1){
          pos += 1
          c = source.charAt(pos)
          if (c.equals('=')) {
            pos += 1
            new Token(EQUALS).setPos(f, pos)
          } else {
            new Token(EQSIGN).setPos(f, pos)
          }
        } else if (c.equals('=')) {
          pos += 1
          new Token(EQSIGN).setPos(f, pos)
        }

        // && case
        if (c.equals('&')) {
          pos += 1
          c = source.charAt(pos)
          if (c.equals('&')){
            new Token(AND).setPos(f, pos)
          } else {
            new Token(BAD).setPos(f, pos)
          }
        }

        // || case
        if (c.equals('|')) {
          pos += 1
          c = source.charAt(pos)
          if (c.equals('|')) {
            new Token(OR).setPos(f, pos)
          } else {
            new Token(BAD).setPos(f, pos)
          }
        }

        // Variable name (aka ID) or keyword case
        var possibleToken = new StringBuilder()
        if (c.isLetter) {
          while (pos < source.length - 1 && c.isLetterOrDigit) {
            possibleToken.append(c)
            pos += 1
            c = source.charAt(pos)
          }
          if (!keywords.contains(possibleToken.toString)){
            pos += 1
            new ID(possibleToken.toString).setPos(f, pos)
          } else {
            new Token(keywords(possibleToken.toString)).setPos(f, pos)
          }
        }

        // Integer case
        if (c.isDigit) {
          while (pos < source.length - 1 && c.isDigit) {
            possibleToken.append(c)
            pos += 1
            c = source.charAt(pos)
          }
          new INTLIT(possibleToken.toInt).setPos(f, pos)
        }

        // String literal case
        if (c.equals('"')) {
          pos += 1
          while (pos < source.length()) {
            c = source.charAt(pos)
            if (c.equals('"')) {
              pos += 1
              new STRLIT(possibleToken.toString).setPos(f, pos)
            }
            possibleToken.append(c)
            pos += 1
          }

        }
        pos += 1
        if (pos >= source.length - 1) {
          eof = true
          new Token(EOF).setPos(f, pos)
        }
        new Token(BAD).setPos(f, pos)
      }
    }

  }
}

object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    new Iterator[Token] {
      def hasNext = {
        tokens.hasNext
      }

      def next = {
        val n = tokens.next
        println(n+"("+n.line+":"+n.col+") ")
        n
      }
    }
  }
}
