package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {

    }

    def parseClassDecl: ClassDecl = {
      var vars: List[VarDecl] = List()
      var methods: List[MethodDecl] = List()
      eat(CLASS)
      var id: Identifier = parseIdentifier
      var parent: Option[Identifier] = null

      // Check if class has parent
      currentToken.kind match {
        case LESSTHAN => {
          // has parent
          eat(LESSTHAN)
          eat(COLON)
          parent = Option(parseIdentifier)
        }
        case _ => parent = Option(null)
      }

      eat(LBRACE)
      // Build up vars
      while (isFirstOfVarDecl) {
        vars = parseVarDecl :: vars
      }
      // Build up methods
      while (isFirstOfMethodDecl) {
        methods = parseMethodDecl :: methods
      }
      eat(RBRACE)
      new ClassDecl(id, parent, vars, methods)
    }

    def parseMainMethod: MainMethod = {
      ???
    }

    def parseIdentifier: Identifier = {
      currentToken.kind match {
        case IDKIND => new Identifier(currentToken.value) // how the fuck get the string?
        case _ => expected(IDKIND)
      }
    }

    def parseVarDecl: VarDecl = {
      eat(VAR)
      val tpe: TypeTree = parseType
      eat(COLON)
      val id: Identifier = parseIdentifier
      eat(SEMICOLON)
      new VarDecl(tpe, id)
    }

    def parseMethodDecl: MethodDecl = {
      eat(METHOD)
      val id: Identifier = parseIdentifier

      eat(LPAREN)
      var args: List[Formal] = List()
      // build up args (which is a list of 'Formal's)
      if (isNextOfFormal) {
        args = parseFormal :: args
        while (isComma) {
          eat(COMMA)
          args = parseFormal :: args
        }
      }
      eat(RPAREN)

      // parse return type
      eat(COLON)
      val retType: TypeTree = parseType

      eat(EQUALS)
      eat(RBRACE)

      // parse var declarations
      var vars: List[VarDecl] = List()
      while (isFirstOfVarDecl) {
        vars = parseVarDecl :: vars
      }

      // parse Expressions
      var exprs: List[ExprTree] = List()
      if (isFirstOfExpression) {
        exprs = 
      }

      new MethodDecl(retType, id, args, vars, exprs, retExpr)
    }

    def parseType: TypeTree = {
      ???
    }

    def parseFormal: Formal = {
      ???
    }




    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
