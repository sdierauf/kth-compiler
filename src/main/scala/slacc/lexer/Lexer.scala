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
          return new Token(EOF)
        }
        
        var c = source.charAt(pos)
        
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
            return new Token(EOF)
          }
          
        }
        
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
            return new Token(DIV)
          }
        }
        
        // simple punctuation type stuff
        if (simpleTokens.contains(c)){
          pos += 1
          return new Token(simpleTokens(c))
        }
        // cases where we have to look ahead
        if (c.equals('=') && pos < source.length() - 1){ 
          pos += 1 
          c = source.charAt(pos)
          if (c.equals('=')) {
            pos += 1
            return new Token(EQUALS)
          } else {
            return new Token(EQSIGN)
          }
        } else if (c.equals('=')) {
          pos += 1
          return new Token(EQSIGN)
        }
        
        if (c.equals('&')) {
          pos += 1 
          c = source.charAt(pos)
          if (c.equals('&')){
            return new Token(AND)
          } else {
            return new Token(BAD)
          }
        }
        
        if (c.equals('|')) {
          pos += 1
          c = source.charAt(pos)
          if (c.equals('|')) {
            return new Token(OR)
          } else {
            return new Token(BAD)
          }
        }
        
        var possibleToken = new StringBuilder()
        if (c.isLetter) {
            while (pos < source.length - 1 && c.isLetterOrDigit) {
              possibleToken.append(c)
              pos += 1
              c = source.charAt(pos)
            }
            if (!keywords.contains(possibleToken.toString)){
              pos += 1
              return new ID(possibleToken.toString)
            } else {
              return new Token(keywords(possibleToken.toString))
            }
        }
        
        if (c.isDigit) {
            while (pos < source.length - 1 && c.isDigit) {
              possibleToken.append(c)
              pos += 1
              c = source.charAt(pos)
            }
            return new INTLIT(possibleToken.toInt)
        }
        
        if (c.equals('"')) {
          pos += 1
          while (pos < source.length()) {
            c = source.charAt(pos)
            if (c.equals('"')) {
              pos += 1 
              return new STRLIT(possibleToken.toString)
            }
            possibleToken.append(c)
            pos += 1
          }
          
        }
        pos += 1
        return new Token(BAD)
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
