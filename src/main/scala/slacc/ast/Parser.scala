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
//      println("want " + kind + ", eating " + currentToken)
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

    def getString(input: Token): String = {
      input match {
        case id: ID => id.value
        case string: STRLIT => string.value
        case _ => fatal("expected something that had a string value (probably building an ID), was " + currentToken.toString)
      }
    }

    def getInt(input: Token): Integer = {
      input match {
        case number: INTLIT => number.value
        case _ => fatal("expected something that had a number value")
      }
    }

    def typeDecl: TypeTree = {
      // Type	::=	Int [ ] || Bool || Int || String || Unit || Identifier
      val x = currentToken
      currentToken.kind match {
        case INT => {
          eat(INT)
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            eat(RBRACKET)
            return new IntArrayType().setPos(x)
          }
          new IntType().setPos(currentToken)
        }
        case UNIT => {
          eat(UNIT)
          new UnitType().setPos(x)
        }
        case BOOLEAN => {
          eat(BOOLEAN)
          new BooleanType().setPos(x)
        }
        case STRING => {
          eat(STRING)
          new StringType().setPos(x)
        }
        case IDKIND => {
          val id = new Identifier(getString(x)).setPos(x)
          eat(IDKIND)
          id
        }
      }
    }

    def varDecl: VarDecl = {
      // VarDeclaration	::=	var Identifier : Type ;
      // VarDecl(tpe: TypeTree, id: Identifier)
      val x = currentToken
      eat(VAR)
      val ident = new Identifier(getString(currentToken)).setPos(currentToken)
      eat(IDKIND)
      eat(COLON)
      val varType = typeDecl
      eat(SEMICOLON)
      new VarDecl(varType, ident).setPos(x)
    }

    def formal: Formal = {
      val argId = new Identifier(getString(currentToken)).setPos(currentToken)
      eat(IDKIND)
      eat(COLON)
      val argType = typeDecl
      new Formal(argType, argId).setPos(argId)
    }

    def mainMethodDecl: MainMethod = { // special case of method declaration where id must eq main
      val x = currentToken
      eat(METHOD)
      // i can do this? *you can now*
      if (!getString(currentToken).equals("main")) {
        fatal("expected: Main method to be called 'Main'")
      }
      eat(IDKIND)
      eat(LPAREN)
      var args : List[Formal] = List()
      if (currentToken.kind == IDKIND) {
        args = args :+ formal
        while (currentToken.kind == COMMA) {
          eat(COMMA)
          args = args :+ formal
        }
      }
      eat(RPAREN)
      eat(COLON)
      val retType = typeDecl
      eat(EQSIGN)
      eat(LBRACE)
      var varDecls : List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        val v = varDecl
        varDecls = varDecls :+ v
      }
      var exprs : List[ExprTree] = List()
      var retExpr: ExprTree = new True().setPos(currentToken) // exprs currently reversed
      while (currentToken.kind != RBRACE) {
        val e = expr
        exprs = e :: exprs
        if (currentToken.kind != RBRACE) {
          eat(SEMICOLON)
        }
      }
      eat(RBRACE)
      if (exprs.nonEmpty) {
        retExpr = exprs.head
        exprs = exprs.tail.reverse // take everything but the 'first' expr, and reverse
      }
      new MainMethod(new MethodDecl(retType, Identifier("main"), args, varDecls, exprs, retExpr).setPos(x)).setPos(x)
    }

    def methodDecl: MethodDecl = {
      // MethodDeclaration	::=	method Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { ( VarDeclaration )* Expression ( ; Expression )* }
      // case class MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree) extends Tree {
      val x = currentToken
      eat(METHOD)
      val ident = new Identifier(getString(currentToken)).setPos(currentToken)
      eat(IDKIND)
      eat(LPAREN)
      var args : List[Formal] = List()
      if (currentToken.kind == IDKIND) {
        args = args :+ formal
        while (currentToken.kind == COMMA) {
          eat(COMMA)
          args = args :+ formal
        }
      }
      eat(RPAREN)
      eat(COLON)
      val retType = typeDecl
      eat(EQSIGN)
      eat(LBRACE)
      var varDecls : List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        val v = varDecl
        varDecls = varDecls :+ v
      }
      var exprs : List[ExprTree] = List()
      while (currentToken.kind != RBRACE) {
        val e = expr
        exprs = e :: exprs
        if (currentToken.kind != RBRACE) {
          eat(SEMICOLON)
        }
      }
      eat(RBRACE)
      val retExpr = exprs.head // exprs currently reversed
      exprs = exprs.tail.reverse // take everything but the 'first' expr, and reverse
      new MethodDecl(retType, ident, args, varDecls, exprs, retExpr).setPos(x)
    }

    def classDecl: ClassDecl = {
      // ClassDeclaration	::=	class Identifier ( <: Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
      // ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree
      val x = currentToken
      eat(CLASS)
      val classIdent = Identifier(getString(currentToken))
      eat(IDKIND)
      var inheritsFrom : Option[Identifier] = None
      if (currentToken.kind == LESSTHAN) {
        eat(LESSTHAN)
        eat(COLON)
        inheritsFrom = Some(new Identifier(getString(currentToken)).setPos(currentToken))
        eat(IDKIND)
      }
      eat(LBRACE)
      var variables : List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        val v = varDecl
        variables = variables :+ v
      }
      var methods : List[MethodDecl] = List()
      while (currentToken.kind == METHOD) {
        val m = methodDecl
        methods = methods :+ m
      }
      eat(RBRACE)
      new ClassDecl(classIdent, inheritsFrom, variables, methods).setPos(x)
    }

    def expr: ExprTree = {
      // expr: andExpr (|| andExpr)*
      var lhs = andExpr
      while (currentToken.kind == OR) {
        eat(OR)
        var rhs = andExpr
        lhs = Or(lhs, rhs) // is this left associative
      }
      lhs
    }

    def andExpr: ExprTree = {
      // andExpr: eqExpr (&& eqExpr)*
      var lhs = eqExpr
      while (currentToken.kind == AND) {
        eat(AND)
        var rhs = eqExpr
        lhs = And(lhs, rhs)
      }
      lhs
    }

    /**
      * From highest priority to lowest: !, then '*' and /, then + and -, then < and ==, then &&, then ||.
      */

    def eqExpr: ExprTree = {
      // eqExpr: compareExpr ((< | ==) compareExpr)*
      var lhs = compareExpr
      while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        if (currentToken.kind == LESSTHAN) {
          eat(LESSTHAN)
          var rhs = compareExpr
          lhs = LessThan(lhs, rhs)
        } else {
          eat(EQUALS)
          var rhs = compareExpr
          lhs = Equals(lhs, rhs)
        }
      }
      lhs
    }

    def compareExpr: ExprTree = {
      // compareExpr: term ((+ | -) term)*
      var lhs = term
      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        if (currentToken.kind == PLUS) {
          eat(PLUS)
          var rhs = term
          lhs = Plus(lhs, rhs)
        } else {
          eat(MINUS)
          var rhs = term
          lhs = Minus(lhs, rhs)
        }
      }
      lhs
    }

    def term: ExprTree = {
      // term: methodCall ((* | /) methodCall)*
      var lhs = methodCall
      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        if (currentToken.kind == TIMES) {
          eat(TIMES)
          var rhs = methodCall
          lhs = Times(lhs, rhs)
        } else {
          eat(DIV)
          var rhs = methodCall
          lhs = Div(lhs, rhs)
        }
      }
      lhs
    }

    def methodCall: ExprTree = {
      // methodCall: factor (. factor)*
      var lhs = factor
      while (currentToken.kind == DOT) {
        eat(DOT)
        if (currentToken.kind == LENGTH) {
          val x = currentToken
          eat(LENGTH) // ????
          return new ArrayLength(lhs).setPos(x)
        } else if (currentToken.kind == IDKIND) {
          // then we have a method call
          val methodName = new Identifier(getString(currentToken)).setPos(currentToken)
          eat(IDKIND)
          var args: List[ExprTree] = List()
          eat(LPAREN)
          while (currentToken.kind != RPAREN) {
            val a = expr
            args = args :+ a
            if (currentToken.kind != RPAREN) eat(COMMA)
          }
          eat(RPAREN)
          lhs = MethodCall(lhs, methodName, args)
        }
      }
      lhs
    }

    def factor: ExprTree = {
      currentToken.kind match {
        case INTLITKIND => {
          val i = IntLit(getInt(currentToken))
          eat(INTLITKIND)
          return i
        }
        case STRLITKIND => {
          val s = StringLit(getString(currentToken))
          eat(STRLITKIND)
          return s
        }
        case LPAREN => {
          eat(LPAREN)
          val e = expr
          eat(RPAREN)
          e
        }
        case LBRACE => {
          val x = currentToken
          eat(LBRACE)
          var expressions : List[ExprTree] = List()
          while (currentToken.kind != RBRACE) {
            val e = expr
            expressions = expressions :+ e
            if (currentToken.kind != RBRACE) {
              eat(SEMICOLON)
            }
          }
          eat(RBRACE)
          new Block(expressions).setPos(x)
        }
        case BANG => {
          val x = currentToken
          eat(BANG)
          val e = methodCall
          new Not(e).setPos(x)
        }
        case IDKIND => {
          val x = currentToken
          val id = getString(currentToken)
          eat(IDKIND)
          currentToken.kind match {
            case LBRACKET => {
              eat(LBRACKET)
              val index = expr
              eat(RBRACKET)
              currentToken.kind match {
                case EQSIGN => {
                  eat(EQSIGN)
                  val assignment = expr
                  new ArrayAssign(new Identifier(id).setPos(x), index, assignment).setPos(x)
                }
                case _ => {
                  new ArrayRead(new Identifier(id).setPos(x), index).setPos(x)
                }
              }
            }
            case EQSIGN => {
              eat(EQSIGN)
              val rhs = expr
              new Assign(new Identifier(id).setPos(x), rhs).setPos(x)
            }
            case _ => new Identifier(id).setPos(x)
          }
        }
        case WHILE => {
          // while ( Expression ) Expression
          val x = currentToken
          eat(WHILE)
          eat(LPAREN)
          val condition = expr
          eat(RPAREN)
          val body = expr
          new While(condition, body).setPos(x)
        }
        case PRINTLN => {
          val x = currentToken
          eat(PRINTLN)
          eat(LPAREN)
          val printExpr = expr
          eat(RPAREN)
          new Println(printExpr).setPos(x)
        }
        case STROF => {
          val x = currentToken
          eat(STROF)
          eat(LPAREN)
          val strOfExpr = expr
          eat(RPAREN)
          new Strof(strOfExpr).setPos(x)
        }
        case IF => {
          val x = currentToken
          eat(IF)
          // if ( Expression ) Expression ( else Expression )?
          //eat(LPAREN)
          val condition = expr
          //eat(RPAREN)
          val thenBody = expr
          var elseBody : Option[ExprTree] = None
          if (currentToken.kind == ELSE) {
            eat(ELSE)
            elseBody = Some(expr)
          }
          new If(condition, thenBody, elseBody).setPos(x)
        }
        case NEW => {
          val x = currentToken
          eat(NEW)
          currentToken.kind match {
            case INT => {
              eat(INT)
              eat(LBRACKET)
              val size = expr
              eat(RBRACKET)
              new NewIntArray(size).setPos(x)
            }
            case IDKIND => {
              val id = new Identifier(getString(currentToken)).setPos(x)
              eat(IDKIND)
              eat(LPAREN)
              eat(RPAREN)
              new New(id)
            }
          }
        }
        case SELF => {
          val x = currentToken
          eat(SELF)
          new Self().setPos(x)
        }
        case TRUE => {
          val x = currentToken
          eat(TRUE)
          new True().setPos(x)
        }
        case FALSE => {
          val x = currentToken
          eat(FALSE)
          new False().setPos(x)
        }
        case _ => {
          fatal("There shouldn't be a token of type " + currentToken.toString + " here!")
        }
      }
    }

    def parseGoal: Program = {
      // Goal	::=	( ClassDeclaration )* MethodDeclaration <EOF>
      // make a list of class declarations then the main method then eat EOF
      var classes: List[ClassDecl] = List()
      while (currentToken.kind == CLASS) {
        val c = classDecl
        classes = classes :+ c
      }

      val mainMethod = mainMethodDecl

      eat(EOF)
      new Program(mainMethod, classes).setPos(mainMethod)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}