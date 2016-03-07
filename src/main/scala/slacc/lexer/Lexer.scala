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

  val keywords = Map(("class", CLASS), ("true", TRUE), ("false", FALSE), ("var", VAR), ("Unit", UNIT),
    ("String", STRING), ("extends", EXTENDS), ("Int", INT), ("Bool", BOOLEAN), ("while", WHILE),
    ("if", IF), ("else", ELSE), ("length", LENGTH), ("self", SELF), ("new", NEW), ("println", PRINTLN),
    ("strOf", STROF), ("method", METHOD))


  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)

    import ctx.reporter._

    new Iterator[Token] {
      var c = source.next
      var pos = source.pos
      var tempPos = pos
      var shouldReturnEOF = false
      var returnedEOF = false
      var inComment = false
      var commentStart = source.pos

      def hasNext : Boolean = {
        // cant use source.hasNext here alone if we want to get the EOF
        if(!source.hasNext && !shouldReturnEOF){
          shouldReturnEOF = true
          return true
        } else if (!source.hasNext && !returnedEOF && shouldReturnEOF) {
          return true
        }  else {
          return source.hasNext 
        }
      }

      def safeInc: Unit = {
        tempPos = pos
        if (source.hasNext) {
          c = source.next
          pos = source.pos
        } 
      }

      def next : Token = {
        // invariant: every case will increment c before the next loop
        if (shouldReturnEOF) {
          returnedEOF = true
          return new Token(EOF).setPos(f, pos)
        }
        if (c.isWhitespace) {
          var isSpaceChar = true
          while (hasNext && isSpaceChar){ // skip white space
            safeInc
            if (!c.isWhitespace){
              isSpaceChar = false
            }
          }
          if (isSpaceChar) { // we've reached the end of the file
            returnedEOF = true
            return new Token(EOF).setPos(f, tempPos)
          }

        }
        
        var tokenStart = source.pos // could be a token, might still be a comment

        if (c.equals('/')){
          commentStart = source.pos
          safeInc
          if (c.equals('/')) {
            while (hasNext && !c.equals('\n')) {
              // single line comment case
              safeInc
            }
          } else if (c.equals('*')) {
            inComment = true
            while (hasNext && inComment) {
              safeInc
              if (c.equals('*') && hasNext){ // have to look ahead
                safeInc
                if (c.equals('/')){
                  inComment = false
                  safeInc
                }
              }
            }
          } else {
            return new Token(DIV).setPos(f, tokenStart)
          }
        }

        if (inComment) { // multiline comment never closed
          fatal("Expected '*/' but found EOF (beginning at " + new Token(BAD).setPos(f, commentStart).position + ").")
        }
        
        tokenStart = source.pos
        // simple punctuation type stuff
        if (simpleTokens.contains(c)) {
          var t = c
          safeInc
          return new Token(simpleTokens(t)).setPos(f, tokenStart)
        }
        // cases where we have to look ahead
        if (c.equals('=') && hasNext) {
          safeInc
          if (c.equals('=')) {
            safeInc
            return new Token(EQUALS).setPos(f, tokenStart)
          } else {
            return new Token(EQSIGN).setPos(f, tokenStart)
          }
        } else if (c.equals('=')) {
          safeInc
          return new Token(EQSIGN).setPos(f, tokenStart)
        }

        if (c.equals('&')) {
          safeInc
          if (c.equals('&')){
            safeInc
            return new Token(AND).setPos(f, tokenStart)
          } else {
            return new Token(BAD).setPos(f, tokenStart)
          }
        }

        if (c.equals('|')) {
          safeInc
          if (c.equals('|')) {
            return new Token(OR).setPos(f, tokenStart)
          } else {
            return new Token(BAD).setPos(f, tokenStart)
          }
        }

        var possibleToken = new StringBuilder()
        if (c.isLetter) {
          while (hasNext && (c.isLetterOrDigit || c == '_')) {
            possibleToken.append(c)
            safeInc
          }
          if (!keywords.contains(possibleToken.toString)) {
            return new ID(possibleToken.toString).setPos(f, tokenStart)
          } else {
            return new Token(keywords(possibleToken.toString)).setPos(f, tokenStart)
          }
        }

        if (c.isDigit) {
          while (hasNext && c.isDigit) {
            possibleToken.append(c)
            safeInc
          }
          return new INTLIT(possibleToken.toInt).setPos(f, tokenStart)
        }

        if (c.equals('"')) {
          while (hasNext) {
            safeInc
            if (c.equals('"')) {
              var t = pos
              safeInc
              return new STRLIT(possibleToken.toString).setPos(f, tokenStart)
            } else {
              possibleToken.append(c)
            }
          }
        }

        if (!hasNext) {
          returnedEOF = true
          return new Token(EOF).setPos(f, tokenStart)
        }

        // somehow nothing happened?
        if (!c.isWhitespace) fatal("Unexpected token  '" + c + "' at " + new Token(BAD).setPos(f, tokenStart).position)

        safeInc
        next

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
