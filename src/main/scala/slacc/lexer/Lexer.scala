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
    val source = Source.fromFile(f)

    import ctx.reporter._

    new Iterator[Token] {
      var c = source.next
      var pos = source.pos

      def hasNext : Boolean = {
        source.hasNext
      }

      def next : Token = {
        // invariant: every case will increment c before the next loop

        if (c.isSpaceChar) {
          var isWhiteSpace = true
          while (hasNext && isWhiteSpace){ // skip white space
            c = source.next
            pos = source.pos
            if (!c.isSpaceChar){
              isWhiteSpace = false
            }
          }
          if (isWhiteSpace) { // we've reached the end of the file
            return new Token(EOF).setPos(f, pos)
          }

        }

        if (c.equals('/')){
          c = source.next
          pos = source.pos
          if (c.equals('/')) {
            while (hasNext && !c.equals('\n')) {
              // single line comment case
              c = source.next
              pos = source.pos
            }
          } else if (c.equals('*')) {
            var finished = false
            while (hasNext && !finished) {
              c = source.next
              pos = source.pos
              if (c.equals('*') && hasNext){ // have to look ahead
                c = source.next
                pos = source.pos
                if (c.equals('/')){
                  finished = true
                }
              }
            }
          } else {
            return new Token(DIV).setPos(f, pos)
          }
        }

        // simple punctuation type stuff
        if (simpleTokens.contains(c)) {
          var t = c
          c = source.next
          pos = source.pos
          return new Token(simpleTokens(t)).setPos(f, pos)
        }
        // cases where we have to look ahead
        if (c.equals('=') && hasNext) {
          c = source.next
          pos = source.pos
          if (c.equals('=')) {
            c = source.next
            pos = source.pos
            return new Token(EQUALS).setPos(f, pos)
          } else {
            return new Token(EQSIGN).setPos(f, pos)
          }
        } else if (c.equals('=')) {
          c = source.next
          pos = source.pos
          return new Token(EQSIGN).setPos(f, pos)
        }

        if (c.equals('&')) {
          c = source.next
          pos = source.pos
          if (c.equals('&')){
            return new Token(AND).setPos(f, pos)
          } else {
            return new Token(BAD).setPos(f, pos)
          }
        }

        if (c.equals('|')) {
          c = source.next
          pos = source.pos
          if (c.equals('|')) {
            return new Token(OR).setPos(f, pos)
          } else {
            return new Token(BAD).setPos(f, pos)
          }
        }

        var possibleToken = new StringBuilder()
        if (c.isLetter) {
          while (hasNext && c.isLetterOrDigit) {
            possibleToken.append(c)
            c = source.next
            pos = source.pos
          }
          if (!keywords.contains(possibleToken.toString)){
            c = source.next
            pos = source.pos
            return new ID(possibleToken.toString).setPos(f, pos)
          } else {
            return new Token(keywords(possibleToken.toString)).setPos(f, pos)
          }
        }

        if (c.isDigit) {
          while (hasNext && c.isDigit) {
            possibleToken.append(c)
            c = source.next
            pos = source.pos
          }
          return new INTLIT(possibleToken.toInt).setPos(f, pos)
        }

        if (c.equals('"')) {
          while (hasNext) {
            c = source.next
            pos = source.pos
            if (c.equals('"')) {
              var t = pos
              c = source.next
              pos = source.pos
              return new STRLIT(possibleToken.toString).setPos(f, t)
            } else {
              possibleToken.append(c)
            }
          }
        }

        if (!hasNext) {
          return new Token(EOF).setPos(f, pos)
        } else {
          c = source.next
          pos = source.pos
          next
        }

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