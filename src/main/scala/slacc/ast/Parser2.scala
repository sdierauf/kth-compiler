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
    
    def getString(input: Token): String = {
      input match {
        case id: ID => id.value
        case string: STRLIT => string.value
        case _ => fatal("expected something that had a string value")
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
      if (currentToken.kind == INT) {
        eat(INT)
        if (currentToken.kind == LBRACKET) {
         eat(LBRACKET) 
         eat(RBRACKET)
         return new IntArrayType()
        }
        new IntType()
      } else if (currentToken.kind == UNIT) {
        eat(UNIT)
        new UnitType()
      } else if (currentToken.kind == BOOLEAN) {
        eat(BOOLEAN)
        new BooleanType()
      } else if (currentToken.kind == STRING) {
        eat(STRING)
        new StringType() 
      }
    }
    
    def varDecl: VarDecl = {
      // VarDeclaration	::=	var Identifier : Type ;
      // VarDecl(tpe: TypeTree, id: Identifier)
      eat(VAR)
      val ident = new Identifier(getString(currentToken))
      eat(COLON)
      val varType = typeDecl;
      eat(SEMICOLON)
      return new VarDecl(varType, ident)
    }
    
    def mainMethodDecl: MethodDecl = { // special case of method declaration where id must eq main
      eat(METHOD)
      eat(ID("main")) // i can do this? 
      eat(LPAREN)
      var args : List[Formal] = List()
      while (currentToken.kind == ID) {
        val argId = Identifier(getString(currentToken))
        eat(ID)
        eat(COLON)
        val argType = typeDecl
        if (currentToken.kind != RPAREN) {
          eat(COMMA)
        }
        args = args::(new Formal(argType, argId)
      }
      eat(RPAREN)
      eat(COLON)
      val retType = typeDecl
      eat(LBRACE)
      var varDecls : List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        val v = varDecl
        varDecls = varDecls::v
      }
      var exprs : List[ExprTree] = List()
      while (currentToken != RBRACE) {
        val e = expr
        exprs = exprs::e
        if (currentToken.kind != RBRACE) {
          eat(SEMICOLON)
        }
      }
      eat(RBRACE)
      val retExpr = exprs.reverse.head // just grab the last expr off the list of exprs 
      return new mainMethod(new methodDecl(retType, Identifier("main"), args, vars, exprs, retExpr)) 
    }
    
    def methodDecl: MethodDecl = {
      // MethodDeclaration	::=	method Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { ( VarDeclaration )* Expression ( ; Expression )* }
      // case class MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree) extends Tree {
      eat(METHOD)
      val ident = new Identifier(currentToken.kind)
      eat(ID)
      eat(LPAREN)
      var args : List[Formal] = List()
      while (currentToken.kind == ID) {
        val argId = Identifier(getString(currentToken))
        eat(ID)
        eat(COLON)
        val argType = typeDecl
        if (currentToken.kind != RPAREN) {
          eat(COMMA)
        }
        args = args::(new Formal(argType, argId)
      }
      eat(RPAREN)
      eat(COLON)
      val retType = typeDecl
      eat(LBRACE)
      var varDecls : List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        val v = varDecl
        varDecls = varDecls::v
      }
      var exprs : List[ExprTree] = List()
      while (currentToken != RBRACE) {
        val e = expr
        exprs = exprs::e
        if (currentToken.kind != RBRACE) {
          eat(SEMICOLON)
        }
      }
      eat(RBRACE)
      val retExpr = exprs.reverse.head 
      return methodDecl(retType, ident, args, vars, exprs, retExpr) 
    }
   
    def classDecl: ClassDecl = {
      // ClassDeclaration	::=	class Identifier ( <: Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
      // ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree
      eat(CLASS)
      val classIdent = Identifier(getString(currentToken))
      eat(ID)
      var inheritsFrom : Option[Identifier] = None
      if (currentToken.kind == LESSTHAN) {
        eat(LESSTHAN) 
        eat(COLON)
        inheritsFrom = Some(getString(currentToken))
        eat(ID)
      }
      eat(LBRACE)
      var variables : List[VarDecl] = List()
      while (currentToken.kind == VAR) {
        val v = varDecl
        variables = variables::v
      }
      var methods : List[MethodDecl] = List()
      while (currentToken.kind == METHOD) {
        val m = methodDecl
        methods = methods::m
      }
      eat(RBRACE)
    }
    
    def expr: ExprTree = { // think method calls should have lowest precedence 
      // expr.length
      // expr.identifier(expr, ..., expr) 
      var lhs = orExpr
      while (currentToken.kind == DOT) {
        eat(DOT)
        if (currentToken.kind == LENGTH) {
           return new ArrayLength(lhs)
        } else if (currentToken.kind == ID) { // then we have a method call
           val methodName = getString(currentToken)
           eat(ID)
           var args : List[ExprTree] = List()
           eat(LPAREN)
           while (currentToken.kind != RPAREN) {
              val a = orExpr
              args = args::a
              if (currentToken.kind != RPAREN) eat(COMMA)
           }
           eat(RPAREN)
           return new MethodCall(lhs, methodName, args)
        }
      }
      
    }
    
    def orExpr: ExprTree = {
      // orExpr: andExpr (|| andExpr)*
      var lhs = andExpr
      while (currentToken.kind == OR) {
        eat(OR)
        var rhs = andExpr 
        lhs = Or(lhs, rhs) // is this left associative 
      }
      return lhs
    }
    
    def andExpr: ExprTree = {
      // andExpr: compareExpr (&& compareExpr)*
      var lhs = compareExpr
      while (currentToken.kind == AND) {
        eat(AND)
        var rhs = compareExpr
        lhs = And(lhs, rhs) 
      }
      return lhs 
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
      return lhs
    }
    
    def term: ExprTree = {
      // term: factor ((* | /) factor)*
      var lhs = factor
      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        if (currentToken.kind == TIMES) {
          eat(TIMES)
          var rhs = factor
          lhs = Times(lhs, rhs)
        } else {
          eat(DIV)
          var rhs = factor
          lhs = Div(lhs, rhs)
        }
      }
    }
    
    def factor: ExprTree = {
      if (currentToken.kind == LPAREN) {
        eat(LPAREN)
        val e = expr 
        eat(RPAREN)
        return e
      } else if (currentToken.kind == LBRACE) {
        eat(LBRACE) 
        var expressions : List[ExprTree] = List()
        while (currentToken.kind != RBRACE) {
          val e = expr
          expressions = expressions::e
          if (currentToken.kind != RBRACE) {
            eat(SEMICOLON)
          }
        }
        eat(RBRACE)
        new Block(expressions)
      }else if (currentToken.kind == BANG) {
        eat(BANG)
        val e = expr
        new Not(e)
      } else if (currentToken.kind == ID) {
        val id = getString(currentToken) 
        eat(ID)
        if (currentToken.kind == LBRACKET) {
          eat(LBRACKET)
          val index = expr 
          eat(RBRACKET)
          eat(EQSIGN) 
          val assignment = expr 
          new ArrayAssign(id, index, assignment) 
        } else if (currentToken.kid == EQSIQN) {
          eat(EQSIGN)
          rhs = expr 
          new Assign(id, rhs)
        } else {
          new Identifier(id) 
        }
      } else if (currentToken.kind == WHILE) {
        // while ( Expression ) Expression
        eat(WHILE)
        eat(LPAREN)
        val condition = expr
        eat(RPAREN)
        val body = expr
        new While(condition, body)
      } else if (currentToken.kind == PRINTLN) {
        eat(PRINTLN)
        eat(LPAREN)
        val printExpr = expr
        eat(RPAREN)
        new Println(printExpr)
      } else if (currentToken.kind == STROF) {
        eat(STROF)
        eat(LPAREN)
        val strOfExpr = expr
        eat(RPAREN)
        new Strof(strOfExpr)
      } else if (currentToken.kind == IF) {
        eat(IF)
        // if ( Expression ) Expression ( else Expression )?
        eat(LPAREN)
        val condition = expr
        eat(RPAREN)
        val thenBody = expr
        val elseBody : Option[ExprTree] = None
        if (currentToken.kind == ELSE) {
          eat(ELSE)
          elseBody = Some(expr)
        }
        new If(condition, thenBody, elseBody)
      } else if (currentToken.kind == NEW) {
        eat(NEW)
        val id = getString(currentToken)
        eat(LPAREN)
        eat(RPAREN)
        new New(id)
      } else if (currentToken.kind == SELF) {
        eat(SELF)
        new Self()
      } else if (currentToken.kind == TRUE) {
        eat(TRUE)
        new True()
      } else if (currentToken.kind == FALSE) {
        eat(FALSE)
        new False()
      }
    }
    
    def parseGoal: Program = {
      // Goal	::=	( ClassDeclaration )* MethodDeclaration <EOF>
      // make a list of class declarations then the main method then eat EOF
      var classes: List[ClassDecl] = List()
      while (currentToken.kind == CLASS) {
        val c = classDecl
        classes = classes::c
      }
      
      val mainMethod = mainMethodDecl
      
      eat(EOF)
      new Program(mainMethod, classes) 
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
