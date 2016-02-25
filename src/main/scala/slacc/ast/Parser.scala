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
      var classes: List[ClassDecl] = List()
      while (isFirstOfClassDecl) {
        classes = classes :+ parseClassDecl
      }
      val main = parseMainMethod
      eat(EOF)
      new Program(main, classes)
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
        vars = vars :+ parseVarDecl
      }
      // Build up methods
      while (isFirstOfMethodDecl) {
        methods = parseMethodDecl :: methods
      }
      eat(RBRACE)
      new ClassDecl(id, parent, vars, methods)
    }

    def parseMainMethod: MainMethod = {
      val main = parseMethodDecl
      if (main.id != Identifier("main")) {
        fatal("expected: main method not identified as 'main'")
      }
      new MainMethod(main)
    }

    def parseIdentifier: Identifier = {
      currentToken.kind match {
        case IDKIND => {
          eat(IDKIND)
          new Identifier(currentToken.value) // how the fuck get the string?
        }
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
        args = args :+ parseFormal
        while (isComma) {
          eat(COMMA)
          args = args :+ parseFormal
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
        vars = vars :+ parseVarDecl
      }

      // parse Expressions
      var exprs: List[ExprTree] = List()
      if (isFirstOfExpression) {
        exprs = parseExpression :: exprs
        while(isSemicolon) {
          eat(SEMICOLON)
          exprs = parseExpression :: exprs
        }
      }

      // now watch me whip
      // now watch me nae nae
      val retExpr = exprs.head
      exprs = exprs.tail.reverse

      new MethodDecl(retType, id, args, vars, exprs, retExpr)
    }

    def parseType: TypeTree = {
      currentToken.kind match {
        case INT => {
          eat(INT)
          currentToken.kind match {
            case LBRACKET => {
              eat(LBRACKET)
              eat(RBRACKET)
              new IntArrayType
            }
            case _ =>
              new IntType
          }
        }
        case BOOLEAN => {
          eat(BOOLEAN)
          new BooleanType
        }
        case STRING => {
          eat(STRING)
          new StringType
        }
        case UNIT => {
          eat(UNIT)
          new UnitType
        }
      }
    }

    def parseFormal: Formal = {
      val id = parseIdentifier
      eat(COLON)
      val tpe = parseType
      new Formal(tpe, id)
    }


    // only parses prefix expressions?
    def parseExpression : ExprTree = currentToken.kind match {
      case STRLITKIND => parseStringLiteral
      case INTLITKIND => parseIntegerLiteral
      case TRUE => parseTrue
      case FALSE => parseFalse
      case IDKIND => parseIdentifier
      case SELF => parseSelf
      case NEW => parseNew
      case BANG => parseBang
      case LPAREN => parseNestedExpression
      case LBRACE => parseBlock
      case IF => parseIf
      case WHILE => parseWhile
      case PRINTLN => parsePrintln
      case STROF => parseStrOf
    }

    def parseStringLiteral: StringLit = {
      ???
    }

    def parseIntegerLiteral: IntLit = {
      ???
    }

    def parseTrue: True = {
      ???
    }

    def parseFalse: False = {
      ???
    }

    def parseSelf: Self = {
      ???
    }

    def parseBang: Not = {
      ???
    }

    def parseNestedExpression: ExprTree = {
      ???
    }

    def parseIf: If = {
      ???
    }

    def parseWhile: While = {
      ???
    }

    def parsePrintln: Println = {
      ???
    }

    def parseStrOf: Strof = {
      ???
    }
    
    def parseBlock: Block = {
      ???
    }




    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
