package scala.gestalt
package parsing

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.BitSet
import Tokens._
import Scanners._
import Chars._
import scala.annotation.{tailrec, switch}

object Parsers {
  type Message = String

  class ParensCounters {
    private var parCounts = new Array[Int](lastParen - firstParen)

    def count(tok: Token) = parCounts(tok - firstParen)
    def change(tok: Token, delta: Int) = parCounts(tok - firstParen) += delta
    def nonePositive: Boolean = parCounts forall (_ <= 0)
  }

  object Location extends Enumeration {
    val InParens, InBlock, InPattern, ElseWhere = Value
  }

  object ParamOwner extends Enumeration {
    val Class, Type, TypeParam, Def = Value
  }

  // unencoded operators
  object raw {
    type N = String
    final val AMP  : N  = "&"
    final val BANG : N  = "!"
    final val BAR  : N  = "|"
    final val DOLLAR: N = "$"
    final val GE: N     = ">="
    final val LE: N     = "<="
    final val MINUS: N  = "-"
    final val NE: N     = "!="
    final val PLUS : N  = "+"
    final val SLASH: N  = "/"
    final val STAR : N  = "*"
    final val TILDE: N  = "~"

    final val isUnary: Set[String] = Set(MINUS, PLUS, TILDE, BANG)
  }

  def precedence(operator: String): Int =
    if (operator eq "<error>") -1
    else {
      val firstCh = operator.head
      if (isScalaLetter(firstCh)) 1
      else if (isOpAssignment(operator)) 0
      else firstCh match {
        case '|' => 2
        case '^' => 3
        case '&' => 4
        case '=' | '!' => 5
        case '<' | '>' => 6
        case ':' => 7
        case '+' | '-' => 8
        case '*' | '/' | '%' => 9
        case _ => 10
      }
    }

  def minPrec = 0
  def minInfixPrec = 1
  def maxPrec = 11


  def isOpAssignment(name: String): Boolean = name match {
    case "!=" | "<=" | ">=" | "" =>
      false
    case _ =>
      name.length > 0 && name.last == '=' && name.head != '=' && isOperatorPart(name.head)
  }


  abstract class Parser(val tb: StructToolbox, val tbName: String, isPattern: Boolean, buf: Array[Char])
  extends TreeHelper {
    import tb._

    val splices: Seq[Tree]

    /* ------------- ERROR HANDLING ------------------------------------------- */
    /** The offset where the last syntax error was reported, or if a skip to a
      *  safepoint occurred afterwards, the offset of the safe point.
      */
    protected var lastErrorOffset : Int = -1

    /** Issue an error at given offset if beyond last error offset
      *  and update lastErrorOffset.
      */
    def syntaxError(msg: => String, offset: Int = in.offset): Unit =
      if (offset > lastErrorOffset) {
        val length = if (in.name != null) in.name.length else 0
        syntaxError(msg)
        lastErrorOffset = in.offset
      }

    /** Unconditionally issue an error at given position, without
      *  updating lastErrorOffset.
      */
    def syntaxError(msg: => String): Unit = ???
      //ctx.error(msg, source atPos pos)

    val in: Scanner = new Scanner(buf)

    val openParens = new ParensCounters

    private var spliceIndex = -1

    def nextSplice: Tree = {
      spliceIndex += 1
      splices(spliceIndex)
    }

    /** This is the general parse entry point.
     *  Overridden by ScriptParser
     */
    /* def parse(): Tree = {
      val t = compilationUnit()
      accept(EOF)
      t
    } */

/* -------------- TOKEN CLASSES ------------------------------------------- */

    def isQuasi = in.token == QUASI
    def rank = in.base

    def isIdent = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT
    def isIdent(name: String) = in.token == IDENTIFIER && in.name == name
    def isSimpleLiteral = simpleLiteralTokens contains in.token
    def isLiteral = literalTokens contains in.token
    def isNumericLit = numericLitTokens contains in.token
    def isModifier = modifierTokens contains in.token
    def isExprIntro = canStartExpressionTokens contains in.token
    def isBindingIntro = canStartBindingTokens contains in.token
    def isTemplateIntro = templateIntroTokens contains in.token
    def isDclIntro = dclIntroTokens contains in.token
    def isStatSeqEnd = in.token == RBRACE || in.token == EOF
    def mustStartStat = mustStartStatTokens contains in.token

    def isDefIntro(allowedMods: BitSet) =
      in.token == AT || (allowedMods contains in.token) || (defIntroTokens contains in.token)

    def isStatSep: Boolean =
      in.token == NEWLINE || in.token == NEWLINES || in.token == SEMI

/* ------------- ERROR HANDLING ------------------------------------------- */

    /** The offset of the last time when a statement on a new line was definitely
     *  encountered in the current scope or an outer scope.
     */
    private var lastStatOffset = -1

    def setLastStatOffset() =
      if (mustStartStat && in.isAfterLineEnd)
        lastStatOffset = in.offset

    /** Is offset1 less or equally indented than offset2?
     *  This is the case if the characters between the preceding end-of-line and offset1
     *  are a prefix of the characters between the preceding end-of-line and offset2.
     */
    /* def isLeqIndented(offset1: Int, offset2: Int): Boolean = {
      def recur(idx1: Int, idx2: Int): Boolean =
        idx1 == offset1 ||
        idx2 < offset2 && source(idx1) == source(idx2) && recur(idx1 + 1, idx2 + 1)
      recur(source.startOfLine(offset1), source.startOfLine(offset2))
    } */

    /** Skip on error to next safe point.
     *  Safe points are:
     *   - Closing braces, provided they match an opening brace before the error point.
     *   - Closing parens and brackets, provided they match an opening parent or bracket
     *     before the error point and there are no intervening other kinds of parens.
     *   - Semicolons and newlines, provided there are no intervening braces.
     *   - Definite statement starts on new lines, provided they are not more indented
     *     than the last known statement start before the error point.
     */
    protected def skip(): Unit = {
      val skippedParens = new ParensCounters
      while (true) {
        (in.token: @switch) match {
          case EOF =>
            return
          case SEMI | NEWLINE | NEWLINES =>
            if (skippedParens.count(LBRACE) == 0) return
          case RBRACE =>
            if (openParens.count(LBRACE) > 0 && skippedParens.count(LBRACE) == 0)
              return
            skippedParens.change(LBRACE, -1)
          case RPAREN =>
            if (openParens.count(LPAREN) > 0 && skippedParens.nonePositive)
              return
            skippedParens.change(LPAREN, -1)
          case RBRACKET =>
            if (openParens.count(LBRACKET) > 0 && skippedParens.nonePositive)
              return
            skippedParens.change(LBRACKET, -1)
          case LBRACE =>
            skippedParens.change(LBRACE, + 1)
          case LPAREN =>
            skippedParens.change(LPAREN, + 1)
          case LBRACKET=>
            skippedParens.change(LBRACKET, + 1)
          case _ =>
            if (mustStartStat && in.isAfterLineEnd())
              return
        }
        in.nextToken()
      }
    }

    def warning(msg: => Message) = ???
      // ctx.warning(msg, sourcePos)

    def deprecationWarning(msg: => Message, offset: Int = in.offset) = ???
      // ctx.deprecationWarning(msg, source atPos Position(offset))

    /** Issue an error at current offset taht input is incomplete */
    def incompleteInputError(msg: => Message) = ???
      // ctx.incompleteInputError(msg, source atPos Position(in.offset))

    /** If at end of file, issue an incompleteInputError.
     *  Otherwise issue a syntax error and skip to next safe point.
     */
   def syntaxErrorOrIncomplete(msg: => Message) =
      if (in.token == EOF) incompleteInputError(msg)
      else {
        syntaxError(msg)
        skip()
        lastErrorOffset = in.offset
      } // DEBUG

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      *
      * @return The offset at the start of the token to accept
      */
    def accept(token: Int): Int = {
      val offset = in.offset
      if (in.token != token) {
        syntaxErrorOrIncomplete(s"unexpected token ${in.name}")
      }
      if (in.token == token) in.nextToken()
      offset
    }

    /** semi = nl {nl} | `;'
     *  nl  = `\n' // where allowed
     */
    def acceptStatSep(): Unit = in.token match {
      case NEWLINE | NEWLINES => in.nextToken()
      case _                  => accept(SEMI)
    }

    def acceptStatSepUnlessAtEnd(altEnd: Token = EOF) =
      if (!isStatSeqEnd && in.token != altEnd) acceptStatSep()


    def errorTermTree    = liftLit(null)

    private var inFunReturnType = false
    private def fromWithinReturnType[T](body: => T): T = {
      val saved = inFunReturnType
      try {
        inFunReturnType = true
        body
      } finally inFunReturnType = saved
    }

    def migrationWarningOrError(msg: String, offset: Int = in.offset) =
      syntaxError(msg, offset)

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

    /** Convert tree to formal parameter list
     */
    def convertToFunctionParams(tree: Tree): List[Tree] = unlift(tree) match {
      case Tuple(ts)  => ts.toList map (convertToFunctionParam(_))
      case t          => convertToFunctionParam(t) :: Nil
    }

    /** Convert tree to formal parameter
     */
    def convertToFunctionParam(tree: Tree, expected: String = "formal parameter"): Tree  = unlift(tree) match {
      case Ident(name) =>
        liftFunctionParam(name, null)
      case Ascribe(Ident(name), tpt) =>
        liftFunctionParam(name, lift(tpt))
      case _ =>
        syntaxError(s"not a legal $expected")
        liftFunctionParam("<error>", null)
    }

/* --------------- PLACEHOLDERS ------------------------------------------- */

    /** The implicit parameters introduced by `_` in the current expression.
     *  Parameters appear in reverse order.
     */
    var placeholderParams: List[Param] = Nil

    def checkNoEscapingPlaceholders[T](op: => T): T = {
      val savedPlaceholderParams = placeholderParams
      placeholderParams = Nil

      try op
      finally {
        placeholderParams match {
          case vd :: _ => syntaxError("unbound placeholder")
          case _ =>
        }
        placeholderParams = savedPlaceholderParams
      }
    }

    def isWildcard(t: Tree): Boolean = unlift(t) match {
      case tb.Ident(name1) => placeholderParams.nonEmpty && name1 == placeholderParams.head.name
      case tb.Ascribe(t1, _) => isWildcard(t1)
      case tb.Annotated(t1, _) => isWildcard(t1)
      case _ => false
    }

/* -------- COMBINATORS -------------------------------------------------------- */

    def enclosed[T](tok: Token, body: => T): T = {
      accept(tok)
      openParens.change(tok, 1)
      try body
      finally {
        accept(tok + 1)
        openParens.change(tok, -1)
      }
    }

    def inParens[T](body: => T): T = enclosed(LPAREN, body)
    def inBraces[T](body: => T): T = enclosed(LBRACE, body)
    def inBrackets[T](body: => T): T = enclosed(LBRACKET, body)

    def inDefScopeBraces[T](body: => T): T = {
      val saved = lastStatOffset
      try inBraces(body)
      finally lastStatOffset = saved
    }

    /** part { `separator` part }
     */
    def tokenSeparated[T](separator: Int, part: () => T): List[T] = {
      val ts = new ListBuffer[T] += part()
      while (in.token == separator) {
        in.nextToken()
        ts += part()
      }
      ts.toList
    }

    def commaSeparated[T](part: () => T): List[T] = tokenSeparated(COMMA, part)

/* --------- OPERAND/OPERATOR STACK --------------------------------------- */
    case class OpInfo(operand: Tree, operator: String, offset: Offset, isType: Boolean)

    /** Is name a left-associative operator? */
    def isLeftAssoc(operator: String) = !operator.isEmpty && (operator.last != ':')

    var opStack: List[OpInfo] = Nil

    def checkAssoc(offset: Token, op1: String, op2: String, op2LeftAssoc: Boolean): Unit =
      if (isLeftAssoc(op1) != op2LeftAssoc)
        syntaxError("mixed left and right associative operator")

    def reduceStack(base: List[OpInfo], top: Tree, prec: Int, leftAssoc: Boolean, op2: String): Tree = {
      if (opStack != base && precedence(opStack.head.operator) == prec)
        checkAssoc(opStack.head.offset, opStack.head.operator, op2, leftAssoc)
      def recur(top: Tree): Tree = {
        if (opStack == base) top
        else {
          val opInfo = opStack.head
          val opPrec = precedence(opInfo.operator)
          if (prec < opPrec || leftAssoc && prec == opPrec) {
            opStack = opStack.tail
            recur {
              if (opInfo.isType) {
                if (opInfo.operator == "&") liftTypeAnd(opInfo.operand, top)
                else if (opInfo.operator == "|") liftTypeOr(opInfo.operand, top)
                else liftTypeApplyInfix(opInfo.operand, opInfo.operator, top)
              }
              else liftInfix(opInfo.operand, opInfo.operator, top)
            }
          }
          else top
        }
      }
      recur(top)
    }

    /** operand { infixop operand} [postfixop],
     *  respecting rules of associativity and precedence.
     *  @param notAnOperator  a token that does not count as operator.
     *  @param maybePostfix   postfix operators are allowed.
     */
    def infixOps(
        first: Tree, canStartOperand: Token => Boolean, operand: () => Tree,
        isType: Boolean = false,
        notAnOperator: String = "",
        maybePostfix: Boolean = false): Tree = {
      val base = opStack
      var top = first
      while (isIdent && in.name != notAnOperator) {
        val op = ident()
        top = reduceStack(base, top, precedence(op), isLeftAssoc(op), op)
        opStack = OpInfo(top, op, in.offset, isType) :: opStack
        newLineOptWhenFollowing(canStartOperand)
        if (maybePostfix && !canStartOperand(in.token)) {
          val topInfo = opStack.head
          opStack = opStack.tail
          val od = reduceStack(base, topInfo.operand, 0, true, in.name)
          return liftPostfix(od, topInfo.operator)
        }
        top = operand()
      }
      reduceStack(base, top, 0, true, in.name)
    }

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

    /** Accept identifier and return its name as a term name. */
    def ident(): String =
      if (isIdent) {
        val name = in.name
        in.nextToken()
        name
      } else {
        syntaxErrorOrIncomplete("Identifier expected")
        "<error>"
      }

    /** Accept identifier and return Ident with its name as a term name. */
    def termIdent(): Tree = liftIdent(ident())

    /** Accept identifier and return Ident with its name as a type name. */
    def typeIdent(): Tree = liftTypeIdent(ident())

    def wildcardIdent(): Tree = {
      accept(USCORE)
      liftIdent("_")
    }

    def termIdentOrWildcard(): Tree =
      if (in.token == USCORE) wildcardIdent() else termIdent()

    /** Accept identifier acting as a selector on given tree `t`. */
    def selector(t: Tree, isType: Boolean): Tree = {
      val name = ident()
      if (in.token == DOT) liftSelect(t, name)
      else if (isType) liftTypeSelect(t, name)  // last part of type select
      else liftSelect(t, name)
    }

    /** Selectors ::= id { `.' id }
     *
     *  Accept `.' separated identifiers acting as a selectors on given tree `t`.
     *  @param finish   An alternative parse in case the next token is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
    def selectors(t: Tree, finish: Tree => Tree, isType: Boolean): Tree = {
      val t1 = finish(t)
      if (t1 ne t) t1 else dotSelectors(selector(t, isType), finish, isType)
    }

    /** DotSelectors ::= { `.' id }
     *
     *  Accept `.' separated identifiers acting as a selectors on given tree `t`.
     *  @param finish   An alternative parse in case the token following a `.' is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
     def dotSelectors(t: Tree, finish: Tree => Tree = id, isType: Boolean) =
      if (in.token == DOT) { in.nextToken(); selectors(t, finish, isType) }
      else t

    private val id: Tree => Tree = x => x

    /** Path       ::= StableId
     *              |  [id `.'] this
     *
     *  @param thisOK   If true, the path can end with the keyword `this`.
     *                  If false, another selection is required after the `this`.
     *  @param finish   An alternative parse in case the token following a `.' is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
    def path(thisOK: Boolean, finish: Tree => Tree = id, isType: Boolean = false): Tree = {
      def handleThis(qual: String) = {
        in.nextToken()
        val t = liftThis(qual)
        if (!thisOK && in.token != DOT) syntaxError("dangling this in path")
        dotSelectors(t, finish, isType)
      }
      def handleSuper(qual: String) = {
        in.nextToken()
        val mix = mixinQualifierOpt()
        val t = liftSuper(qual, mix)
        accept(DOT)
        dotSelectors(selector(t, isType), finish, isType)
      }
      if (in.token == THIS) handleThis("")
      else if (in.token == SUPER) handleSuper("")
      else {
        val t = ident()
        if (in.token == DOT) {
          in.nextToken()
          if (in.token == THIS) handleThis(t)
          else if (in.token == SUPER) handleSuper(t)
          else selectors(liftIdent(t), finish, isType)
        }
        else if (isType) liftTypeIdent(t)
        else liftIdent(t)
      }
    }

    /** MixinQualifier ::= `[' id `]'
    */
    def mixinQualifierOpt(): String =
      if (in.token == LBRACKET) inBrackets(ident())
      else ""

    /** StableId ::= id
     *            |  Path `.' id
     *            |  [id '.'] super [`[' id `]']`.' id
     */
    def stableId(): Tree =
      path(thisOK = false)

    /** QualId ::= id {`.' id}
    */
    def qualId(): Tree =
      dotSelectors(termIdent(), isType = false)

    /** SimpleExpr    ::= literal
     *                  | symbol
     *                  | null
     *  @param negOffset   The offset of a preceding `-' sign, if any.
     *                     If the literal is not negated, negOffset = in.offset.
     */
    def literal(negOffset: Int = in.offset, inPattern: Boolean = false): Tree = {
      def finish(value: Any): Tree = {
        val t = liftLit(value)
        in.nextToken()
        t
      }
      val isNegated = negOffset < in.offset

      if (in.token == SYMBOLLIT) {
        syntaxError("symbol literal not supported")
        null
        // atPos(in.skipToken()) { SymbolLit(in.strVal) }
      }
      else if (in.token == INTERPOLATIONID) interpolatedString(inPattern)
      else finish(in.token match {
        case CHARLIT                => in.charVal
        case INTLIT                 => in.intVal(isNegated).toInt
        case LONGLIT                => in.intVal(isNegated)
        case FLOATLIT               => in.floatVal(isNegated).toFloat
        case DOUBLELIT              => in.floatVal(isNegated)
        case STRINGLIT | STRINGPART => in.strVal
        case TRUE                   => true
        case FALSE                  => false
        case NULL                   => null
        case _                      =>
          syntaxErrorOrIncomplete("invalid literal")
          null
      })
    }

    private def interpolatedString(inPattern: Boolean = false): Tree = {
      val partsBuf = new ListBuffer[String]
      val argsBuf = new ListBuffer[Tree]
      val interpolator = in.name
      in.nextToken()
      while (in.token == STRINGPART) {
        in.nextToken()
        val part = in.strVal
        partsBuf += part

        argsBuf += (
        if (in.token == IDENTIFIER)
          termIdent()
        else if (in.token == USCORE && inPattern) {
          in.nextToken()
          liftIdent("_")
        }
        else if (in.token == THIS) {
          in.nextToken()
          liftThis("")
        }
        else if (in.token == LBRACE)
          if (inPattern) {
            // liftBlock(List(inBraces(pattern())))
            syntaxError("Patterns in interpolated string not supported yet")
            liftIdent("<error>")
          }
          else {
            // expr()
            syntaxError("Expr in interpolated string not supported yet")
            liftIdent("<error>")
          }
        else {
          syntaxError("error in interpolated string: identifier or block expected")
          liftIdent("<error>")
        })
      }
      if (in.token == STRINGLIT) {
        in.nextToken()
        val part = in.strVal
        partsBuf += part
      }

      liftInterpolate(interpolator, partsBuf.toList, argsBuf.toList)
    }

/* ------------- NEW LINES ------------------------------------------------- */

    def newLineOpt(): Unit = {
      if (in.token == NEWLINE) in.nextToken()
    }

    def newLinesOpt(): Unit = {
      if (in.token == NEWLINE || in.token == NEWLINES)
        in.nextToken()
    }

    def newLineOptWhenFollowedBy(token: Int): Unit = {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && in.next.token == token) newLineOpt()
    }

    def newLineOptWhenFollowing(p: Int => Boolean): Unit = {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && p(in.next.token)) newLineOpt()
    }

/* ------------- TYPES ------------------------------------------------------ */
    /** Same as [[typ]], but if this results in a wildcard it emits a syntax error and
     *  returns a tree for type `Any` instead.
     */
    def toplevelTyp(): Tree = {
      // TODO: some checking required, add a flag to `typ()`.
      typ()
    }

    /** Type        ::=  [`implicit'] FunArgTypes `=>' Type
     *                |  HkTypeParamClause `->' Type
     *                |  InfixType
     *  FunArgTypes ::=  InfixType
     *                |  `(' [ FunArgType {`,' FunArgType } ] `)'
     */
    def typ(): Tree = {
      val start = in.offset
      val isImplicit = in.token == IMPLICIT
      if (isImplicit) in.nextToken()
      def functionRest(params: List[Tree]): Tree = {
        accept(ARROW)
        val t = typ()
        if (isImplicit) {
          syntaxError("implicit function type not supported yet")
          null
        }
        else liftTypeFunction(params, t)
      }
      val t =
        if (in.token == LPAREN) {
          in.nextToken()
          if (in.token == RPAREN) {
            in.nextToken()
            functionRest(Nil)
          }
          else {
            openParens.change(LPAREN, 1)
            val ts = commaSeparated(funArgType)
            openParens.change(LPAREN, -1)
            accept(RPAREN)
            if (isImplicit || in.token == ARROW) functionRest(ts)
            else {
              val tuple = if (ts.size == 1) ts.head else liftTuple(ts)
              infixTypeRest(
                refinedTypeRest(
                  withTypeRest(
                    annotTypeRest(
                      simpleTypeRest(tuple)))))
            }
          }
        }
        else if (in.token == LBRACKET) {
          // val tparams = typeParamClause(ParamOwner.TypeParam)
          // if (in.token == ARROW)
             // LambdaTypeTree(tparams, typ())
          // else { accept(ARROW); typ() }
          syntaxError("lambda type tree not supported yet")
          null
        }
        else infixType()

      in.token match {
        case ARROW => functionRest(t :: Nil)
        case FORSOME => syntaxError("Existential types not supported"); t
        case _ => t
      }
    }

    /** InfixType ::= RefinedType {id [nl] refinedType}
     */
    def infixType(): Tree = infixTypeRest(refinedType())

    def infixTypeRest(t: Tree): Tree =
      infixOps(t, canStartTypeTokens, refinedType, isType = true, notAnOperator = "*")

    /** RefinedType        ::=  WithType {Annotation | [nl] Refinement}
     */
    val refinedType: () => Tree = () => refinedTypeRest(withType())

    def refinedTypeRest(t: Tree): Tree = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) refinedTypeRest(liftTypeRefine(t, refinement()))
      else t
    }

    /** WithType ::= AnnotType {`with' AnnotType}    (deprecated)
     */
    def withType(): Tree = withTypeRest(annotType())

    def withTypeRest(t: Tree): Tree =
      if (in.token == WITH) {
        deprecationWarning("with is deprecated")
        in.nextToken()
        liftTypeAnd(t, withType())
      }
      else t

    /** AnnotType ::= SimpleType {Annotation}
     */
    def annotType(): Tree = annotTypeRest(simpleType())

    def annotTypeRest(t: Tree, annots: Seq[Tree] = Nil): Tree =
      if (in.token == AT) {
        annotTypeRest(t, annots :+ annot())
        t
      }
      else if (annots.size > 0) liftTypeAnnotated(t, annots)
      else t

    /** SimpleType       ::=  SimpleType TypeArgs
     *                     |  SimpleType `#' id
     *                     |  StableId
     *                     |  Path `.' type
     *                     |  `(' ArgTypes `)'
     *                     |  `_' TypeBounds
     *                     |  Refinement
     *                     |  Literal
     */
    def simpleType(): Tree = simpleTypeRest {
      if (in.token == LPAREN) {
        val tuple = inParens(argTypes(namedOK = false, wildOK = true))
        if (tuple.size == 0) tuple.head else liftTuple(tuple)
      }
      else if (in.token == LBRACE)
        liftTypeRefine(null, refinement())
      else if (isSimpleLiteral) { liftTypeSingleton(literal()) }
      else if (in.token == USCORE) {
        val start = in.skipToken()
        typeBounds()
      }
      else path(thisOK = false, handleSingletonType, isType = true)
    }

    val handleSingletonType: Tree => Tree = t =>
      if (in.token == TYPE) {
        in.nextToken()
        liftTypeSingleton(t)
      } else t

    private def simpleTypeRest(t: Tree): Tree = in.token match {
      case HASH =>
        // simpleTypeRest(typeProjection(t))
        syntaxError("type projection not supported")
        null
      case LBRACKET =>
        simpleTypeRest(
          liftTypeApply(t, typeArgs(namedOK = false, wildOK = true))
        )
      case _ => t
    }

    private def typeProjection(t: Tree): Tree = {
      accept(HASH)
      val id = ident()
      liftTypeSelect(t, id)
    }

    /** NamedTypeArg      ::=  id `=' Type
     */
    val namedTypeArg = () => {
      val name = ident()
      accept(EQUALS)
      // NamedArg(name.toTypeName, typ())
    }

    /**   ArgTypes          ::=  Type {`,' Type}
     *                        |  NamedTypeArg {`,' NamedTypeArg}
     */
    def argTypes(namedOK: Boolean, wildOK: Boolean): List[Tree] = {
      def otherArgs(first: Tree, arg: () => Tree): List[Tree] = {
        val rest =
          if (in.token == COMMA) {
            in.nextToken()
            commaSeparated(arg)
          }
          else Nil
        first :: rest
      }
      def typParser() = if (wildOK) typ() else toplevelTyp()
      if (namedOK && in.token == IDENTIFIER)
        typParser() match {
          case _ if in.token == EQUALS =>
            in.nextToken()
            // otherArgs(NamedArg(name, typ()), namedTypeArg)
            syntaxError("Named type arguments not supported yet")
            Nil
          case firstArg =>
            if (in.token == EQUALS) println(s"??? $firstArg")
            otherArgs(firstArg, typ)
        }
      else commaSeparated(typParser)
    }

    /** FunArgType ::=  Type | `=>' Type
     */
    val funArgType = () =>
      if (in.token == ARROW) { in.skipToken();  liftTypeByName(typ()) }
      else typ()

    /** ParamType ::= [`=>'] ParamValueType
     */
    def paramType(): Tree =
      if (in.token == ARROW) liftTypeByName(paramValueType())
      else paramValueType()

    /** ParamValueType ::= Type [`*']
     */
    def paramValueType(): Tree = {
      val t = toplevelTyp()
      if (isIdent("*")) {
        in.nextToken()
        liftTypeRepeated(t)
      } else t
    }

    /** TypeArgs      ::= `[' Type {`,' Type} `]'
     *  NamedTypeArgs ::= `[' NamedTypeArg {`,' NamedTypeArg} `]'
     */
    def typeArgs(namedOK: Boolean, wildOK: Boolean): List[Tree] = inBrackets(argTypes(namedOK, wildOK))

    /** Refinement ::= `{' RefineStatSeq `}'
     */
    def refinement(): List[Tree] = {
      syntaxError("Refinement not supported yet")
      // inBraces(refineStatSeq())
      Nil
    }

    /** TypeBounds ::= [`>:' Type] [`<:' Type]
     */
    def typeBounds(): Tree = {
      val lo = bound(SUPERTYPE)
      val hi = bound(SUBTYPE)
      if (lo == null && hi == null) null
      else liftTypeBounds(lo, hi)
    }

    private def bound(tok: Int): Tree =
      if (in.token == tok) { in.nextToken(); toplevelTyp() }
      else null

    /** TypeParamBounds   ::=  TypeBounds {`<%' Type} {`:' Type}
     */
    def typeParamBounds(pname: String): Tree = {
      val t = typeBounds()
      val cbs = contextBounds(pname)
      if (cbs.isEmpty) t
      else {
        // TODO: context bounds
        // ContextBounds(t, cbs)
        syntaxError("Context bounds not supported")
        null
      }
    }

    def contextBounds(pname: String): List[Tree] = in.token match {
      case COLON =>
         liftTypeApply(toplevelTyp(), List(liftTypeIdent(pname))) :: contextBounds(pname)
      case VIEWBOUND =>
        syntaxError("view bounds `<%' not supported, use a context bound `:' instead")
        Nil
        // atPos(in.skipToken) {
        //  Function(Ident(pname) :: Nil, toplevelTyp())
        // } :: contextBounds(pname)
      case _ =>
        Nil
    }

    def typedOpt(): Tree =
      if (in.token == COLON) { in.nextToken(); toplevelTyp() }
      else null

    def typeDependingOn(location: Location.Value): Tree =
      if (location == Location.InParens) typ()
      else if (location == Location.InPattern) refinedType()
      else infixType()

/* ----------- EXPRESSIONS ------------------------------------------------ */

    /** EqualsExpr ::= `=' Expr
     */
    def equalsExpr(): Tree = {
      accept(EQUALS)
      expr()
    }

    def condExpr(altToken: Token): Tree = {
      if (in.token == LPAREN) {
        val t = inParens(exprInParens())
        if (in.token == altToken) in.nextToken()
        t
      } else {
        val t = expr()
        accept(altToken)
        t
      }
    }

    /** Expr              ::=  [`implicit'] FunParams `=>' Expr
     *                      |  Expr1
     *  FunParams         ::=  Bindings
     *                      |  id
     *                      |  `_'
     *  ExprInParens      ::=  PostfixExpr `:' Type
     *                      |  Expr
     *  BlockResult       ::=  [`implicit'] FunParams `=>' Block
     *                      |  Expr1
     *  Expr1             ::=  `if' `(' Expr `)' {nl} Expr [[semi] else Expr]
     *                      |  `if' Expr `then' Expr [[semi] else Expr]
     *                      |  `while' `(' Expr `)' {nl} Expr
     *                      |  `while' Expr `do' Expr
     *                      |  `do' Expr [semi] `while' Expr
     *                      |  `try' Expr Catches [`finally' Expr]
     *                      |  `try' Expr [`finally' Expr]
     *                      |  `throw' Expr
     *                      |  `return' [Expr]
     *                      |  ForExpr
     *                      |  [SimpleExpr `.'] id `=' Expr
     *                      |  SimpleExpr1 ArgumentExprs `=' Expr
     *                      |  PostfixExpr [Ascription]
     *                      |  PostfixExpr `match' `{' CaseClauses `}'
     *  Bindings          ::= `(' [Binding {`,' Binding}] `)'
     *  Binding           ::= (id | `_') [`:' Type]
     *  Ascription        ::= `:' CompoundType
     *                      | `:' Annotation {Annotation}
     *                      | `:' `_' `*'
     */
    val exprInParens = () => expr(Location.InParens)

    def expr(): Tree = expr(Location.ElseWhere)

    def expr(location: Location.Value): Tree = {
      val start = in.offset
      if (in.token == IMPLICIT) {
        // TODO: implicit function
        // implicitClosure(start, location, implicitMods())
        syntaxError("Implicit closure not supported yet")
        null
      }
      else {
        val saved = placeholderParams
        placeholderParams = Nil

        def wrapPlaceholders(t: Tree) = try
          if (placeholderParams.isEmpty) t
          else liftFunction(placeholderParams.reverse.map(lift), t)
        finally placeholderParams = saved

        val t = expr1(location)
        if (in.token == ARROW) {
          placeholderParams = Nil // don't interpret `_' to the left of `=>` as placeholder
          wrapPlaceholders(closureRest(start, location, convertToFunctionParams(t)))
        }
        else if (isWildcard(t)) {
          placeholderParams = placeholderParams ::: saved
          t
        }
        else wrapPlaceholders(t)
      }
    }

    def expr1(location: Location.Value = Location.ElseWhere): Tree = in.token match {
      case IF =>
        in.skipToken()
        val cond = condExpr(THEN)
        newLinesOpt()
        val thenp = expr()
        val elsep = if (in.token == ELSE) { in.nextToken(); expr() }
                    else null
        liftIf(cond, thenp, elsep)
      case WHILE =>
        in.skipToken()
        val cond = condExpr(DO)
        newLinesOpt()
        val body = expr()
        liftWhile(cond, body)
      case DO =>
        in.skipToken()
        val body = expr()
        if (isStatSep) in.nextToken()
        accept(WHILE)
        val cond = expr()
        liftDoWhile(body, cond)
      case TRY =>
        val tryOffset = in.offset
        in.skipToken()
        val body = expr()
        val handler =
          if (in.token == CATCH) {
            in.nextToken()
            expr()
          } else null

        if (handler != null) unlift(handler) match {
          case tb.Block(Nil) =>
            syntaxError("empty catch block")
          case _ =>
        }

        val finalizer =
          if (in.token == FINALLY) { accept(FINALLY); expr() }
          else {
            if (handler == null) warning("empty catch and finally block")
            null
          }
        if (handler == null) liftTry(body, Nil, finalizer)
        else unlift(handler) match {
          case tb.Block(cases) => liftTry(body, cases.map(lift), finalizer)
          case _ => liftTry(body, handler, finalizer)
        }
      case THROW =>
        in.skipToken()
        liftThrow(expr())
      case RETURN =>
        in.skipToken()
        if (isExprIntro) liftReturn(expr() )
        else liftReturn
      case FOR =>
        forExpr()
      case _ =>
        expr1Rest(postfixExpr(), location)
    }

    def expr1Rest(t: Tree, location: Location.Value) = in.token match {
      case EQUALS =>
         unlift(t) match {
           case tb.Ident(_) | tb.Select(_, _) | tb.Apply(_, _) =>
             in.skipToken()
             Assign(t, expr())
           case _ =>
             t
         }
      case COLON =>
        ascription(t, location)
      case MATCH =>
        in.skipToken()
        inBraces(liftMatch(t, caseClauses()))
      case _ =>
        t
    }

    def ascription(t: Tree, location: Location.Value) = {
      in.skipToken()
      in.token match {
        case USCORE =>
          val uscoreStart = in.skipToken()
          if (isIdent("*")) {
            in.nextToken()
            if (in.token != RPAREN) syntaxError("`_*' can be used only for last argument")
            liftAscribe(t, liftTypeIdent("_*"))
          } else {
            syntaxErrorOrIncomplete("incorrect repeated argument syntax")
            t
          }
        case AT if location != Location.InPattern =>
          liftAnnotated(t,  annotations())
        case _ =>
          val tpt = typeDependingOn(location)
          if (isWildcard(t) && location != Location.InPattern) {
            val vd :: rest = placeholderParams
            placeholderParams =
              vd.copy(tptOpt = Some(unliftTypeTree(tpt))) :: rest
          }
          liftAscribe(t, tpt)
      }
    }

    /** FunParams         ::=  Bindings
     *                     |   id
     *                     |   `_'
     *  Bindings          ::=  `(' [Binding {`,' Binding}] `)'
     */
    def funParams(mods: Mods, location: Location.Value): List[Tree] =
      if (in.token == LPAREN)
        inParens(if (in.token == RPAREN) Nil else commaSeparated(() => binding(mods)))
      else {
        val start = in.offset
        val name = bindingName()
        val t =
          if (in.token == COLON && location == Location.InBlock) {
            in.nextToken()
            infixType()
          }
          else null
        liftParam(mods, name, t, null) :: Nil
      }

    /**  Binding           ::= (id | `_') [`:' Type]
     */
    def binding(mods: Mods): Tree =
      liftParam(mods, bindingName(), typedOpt(), null)

    def bindingName(): String =
      if (in.token == USCORE) {
        in.nextToken()
        fresh("_$")
      }
      else ident()

    /** Expr         ::= implicit id `=>' Expr
     *  BlockResult  ::= implicit id [`:' InfixType] `=>' Block // Scala2 only
     */
    def implicitClosure(start: Int, location: Location.Value, implicitMods: Mods): Tree =
      closureRest(start, location, funParams(implicitMods, location))

    def closureRest(start: Int, location: Location.Value, params: List[Tree]): Tree = {
      accept(ARROW)
      liftFunction(params, if (location == Location.InBlock) block() else expr())
    }

    /** PostfixExpr   ::= InfixExpr [id [nl]]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr id [nl] InfixExpr
     */
    def postfixExpr(): Tree =
      infixOps(prefixExpr(), canStartExpressionTokens, prefixExpr, maybePostfix = true)

    /** PrefixExpr   ::= [`-' | `+' | `~' | `!'] SimpleExpr
     */
    val prefixExpr = () =>
      if (isIdent && raw.isUnary(in.name)) {
        val start = in.offset
        val op = ident()
        if (op == raw.MINUS && isNumericLit)
          simpleExprRest(literal(start), canApply = true)
        else
          liftPrefix(op, simpleExpr())
      }
      else simpleExpr()

    /** SimpleExpr    ::= new Template
     *                 |  BlockExpr
     *                 |  SimpleExpr1 [`_']
     *  SimpleExpr1   ::= literal
     *                 |  xmlLiteral
     *                 |  Path
     *                 |  `(' [ExprsInParens] `)'
     *                 |  SimpleExpr `.' id
     *                 |  SimpleExpr (TypeArgs | NamedTypeArgs)
     *                 |  SimpleExpr1 ArgumentExprs
     */
    def simpleExpr(): Tree = {
      var canApply = true
      val t = in.token match {
        case XMLSTART =>
          syntaxError("XML literal not supported")
          null
        case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER =>
          path(thisOK = true)
        case USCORE =>
          in.skipToken()
          val pname = fresh("_$")
          val param = liftFunctionParam(pname, null)
          placeholderParams = unlift(param).asInstanceOf[Param] :: placeholderParams
          liftIdent(pname)
        case LPAREN =>
          inParens(exprsInParensOpt()) match {
            case t :: Nil => t
            case ts => liftTuple(ts)
          }
        case LBRACE =>
          canApply = false
          blockExpr()
        case NEW =>
          canApply = false
          in.skipToken()

          val (parents, self, stats) = templateOpt
          val tpe =
            parents match {
              case parent :: Nil if self == null && stats.isEmpty =>
                parent
              case _ =>
                liftAnonymClass(parents, self, stats)
            }

          liftNew(tpe)
        case _ =>
          if (isLiteral) literal()
          else {
            syntaxErrorOrIncomplete("illegal start of simple expression:" + tokenString(in.token))
            errorTermTree
            null
          }
      }
      simpleExprRest(t, canApply)
    }

    def simpleExprRest(t: Tree, canApply: Boolean = true): Tree = {
      if (canApply) newLineOptWhenFollowedBy(LBRACE)
      in.token match {
        case DOT =>
          in.nextToken()
          simpleExprRest(selector(t, isType = false), canApply = true)
        case LBRACKET =>
          val tapp = liftTypeApply(t, typeArgs(namedOK = true, wildOK = false))
          simpleExprRest(tapp, canApply = true)
        case LPAREN | LBRACE if canApply =>
          val app = liftApply(t, argumentExprs())
          simpleExprRest(app, canApply = true)
        case USCORE =>
          in.skipToken()
          liftPostfix(t, "_") // TODO: eta expansion should be a separate tree?
        case _ =>
          t
      }
    }

    /**   ExprsInParens     ::=  ExprInParens {`,' ExprInParens}
     */
    def exprsInParensOpt(): List[Tree] =
      if (in.token == RPAREN) Nil else commaSeparated(exprInParens)

    /** ParArgumentExprs ::= `(' [ExprsInParens] `)'
     *                    |  `(' [ExprsInParens `,'] PostfixExpr `:' `_' `*' ')' \
     */
    def parArgumentExprs(): List[Tree] =
      inParens(if (in.token == RPAREN) Nil else commaSeparated(argumentExpr))

    /** ArgumentExprs ::= ParArgumentExprs
     *                 |  [nl] BlockExpr
     */
    def argumentExprs(): List[Tree] =
      if (in.token == LBRACE) blockExpr() :: Nil else parArgumentExprs()

    val argumentExpr = () => unlift(exprInParens()) match {
      case a @ Assign(Ident(id), rhs) => liftNamed(id, lift(rhs))
      case e => e
    }

    /** ArgumentExprss ::= {ArgumentExprs}
     */
    def argumentExprss(fn: Tree): Tree = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LPAREN || in.token == LBRACE) argumentExprss(Apply(fn, argumentExprs()))
      else fn
    }

    /** ParArgumentExprss ::= {ParArgumentExprs}
     */
    def parArgumentExprss(fn: Tree): Tree =
      if (in.token == LPAREN) parArgumentExprss(Apply(fn, parArgumentExprs()))
      else fn

    /** BlockExpr ::= `{' (CaseClauses | Block) `}'
     */
    def blockExpr(): Tree =
      inDefScopeBraces {
        if (in.token == CASE) liftBlock(caseClauses())
        else block()
      }

    /** Block ::= BlockStatSeq
     *  @note  Return tree does not carry source position.
     */
    def block(): Tree = {
      val stats = blockStatSeq()
      liftBlock(stats)
    }

    /** Guard ::= if PostfixExpr
     */
    def guard(): Tree =
      if (in.token == IF) { in.nextToken(); postfixExpr() }
      else null

    /** Enumerators ::= Generator {semi Enumerator | Guard}
     */
    def enumerators(): List[Tree] = generator() :: enumeratorsRest()

    def enumeratorsRest(): List[Tree] =
      if (isStatSep) { in.nextToken(); enumerator() :: enumeratorsRest() }
      else if (in.token == IF) guard() :: enumeratorsRest()
      else Nil

    /** Enumerator  ::=  Generator
     *                |  Guard
     *                |  Pattern1 `=' Expr
     */
    def enumerator(): Tree =
      if (in.token == IF) guard()
      else {
        val pat = pattern1()
        if (in.token == EQUALS) {
          in.skipToken()
          liftGenAlias(pat, expr())
        }
        else generatorRest(pat)
      }

    /** Generator   ::=  Pattern `<-' Expr
     */
    def generator(): Tree = generatorRest(pattern1())

    def generatorRest(pat: Tree) = {
      accept(LARROW)
      liftGenFrom(pat, expr())
    }

    /** ForExpr  ::= `for' (`(' Enumerators `)' | `{' Enumerators `}')
     *                {nl} [`yield'] Expr
     *            |  `for' Enumerators (`do' Expr | `yield' Expr)
     */
    def forExpr(): Tree = {
      in.skipToken()
      var wrappedEnums = true
      val enums =
        if (in.token == LBRACE) inBraces(enumerators())
        else if (in.token == LPAREN) {
          val lparenOffset = in.skipToken()
          openParens.change(LPAREN, 1)
          val pats = patternsOpt()
          val pat =
            if (in.token == RPAREN || pats.length > 1) {
              wrappedEnums = false
              accept(RPAREN)
              openParens.change(LPAREN, -1)
              pats match {
                case t :: Nil => t
                case _ => liftTuple(pats)
              }
            }
            else pats.head
          val res = generatorRest(pat) :: enumeratorsRest()
          if (wrappedEnums) {
            accept(RPAREN)
            openParens.change(LPAREN, -1)
          }
          res
        } else {
          wrappedEnums = false
          enumerators()
        }
      newLinesOpt()
      if (in.token == YIELD) { in.nextToken(); liftFor(enums, liftYield(expr())) }
      else if (in.token == DO) { in.nextToken(); liftFor(enums, expr()) }
      else {
        if (!wrappedEnums) syntaxErrorOrIncomplete(""""yield" or "do" expected""")
        liftFor(enums, expr())
      }
    }

    /** CaseClauses ::= CaseClause {CaseClause}
    */
    def caseClauses(): List[Tree] = {
      val buf = new ListBuffer[Tree]
      buf += caseClause()
      while (in.token == CASE) buf += caseClause()
      buf.toList
    }

    /** CaseClause ::= case Pattern [Guard] `=>' Block
    */
    def caseClause(): Tree = {
      accept(CASE)
      liftCase(pattern(), guard(), { accept(ARROW); block() })
    }

    /* -------- PATTERNS ------------------------------------------- */

    /**  Pattern           ::=  Pattern1 { `|' Pattern1 }
     */
    val pattern = () => {
      val pat = pattern1()
      if (isIdent(raw.BAR))
        liftAlternative(pat :: patternAlts())
      else pat
    }

    def patternAlts(): List[Tree] =
      if (isIdent(raw.BAR)) { in.nextToken(); pattern1() :: patternAlts() }
      else Nil

    // TODO: check backquoted ident
    def isVarPattern(tree: Tree): Boolean = tree match {
      case Ident(_) => true
      case _ => false
    }

    /**  Pattern1          ::= PatVar Ascription
     *                      |  Pattern2
     */
    def pattern1(): Tree = {
      val p = pattern2()
      if (isVarPattern(unlift(p)) && in.token == COLON) ascription(p, Location.InPattern)
      else p
    }

    /**  Pattern2    ::=  [varid `@'] InfixPattern
     */
    val pattern2 = () => unlift(infixPattern()) match {
      case p @ Ident(name) if isVarPattern(p) && in.token == AT =>
        in.skipToken()
        liftBind(name, infixPattern())
      case p => lift(p)
    }

    /**  InfixPattern ::= SimplePattern {id [nl] SimplePattern}
     */
    def infixPattern(): Tree =
      infixOps(simplePattern(), canStartExpressionTokens, simplePattern, notAnOperator = raw.BAR)

    /** SimplePattern    ::= PatVar
     *                    |  Literal
     *                    |  XmlPattern
     *                    |  `(' [Patterns] `)'
     *                    |  SimplePattern1 [TypeArgs] [ArgumentPatterns]
     *  SimplePattern1   ::= Path
     *                    |  `{' Block `}'
     *                    |  SimplePattern1 `.' id
     *  PatVar           ::= id
     *                    |  `_'
     */
    val simplePattern = () => in.token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
        path(thisOK = true) match {
          case id @ Ident(raw.MINUS) if isNumericLit => literal()
          case t => simplePatternRest(t)
        }
      case USCORE =>
        wildcardIdent()
      case LPAREN =>
        inParens(patternsOpt()) match {
          case t :: Nil => t
          case ts => liftTuple(ts)
        }
      case LBRACE =>
        dotSelectors(blockExpr(), isType = false)
      case XMLSTART =>
        // xmlLiteralPattern()
        syntaxError("XML literal pattern not supported")
        null
      case _ =>
        if (isLiteral) literal(inPattern = true)
        else {
          syntaxErrorOrIncomplete("illegal start of simple pattern")
          null
        }
    }

    def simplePatternRest(t: Tree): Tree = {
      var p = t
      if (in.token == LBRACKET)
        p = liftTypeApply(p, typeArgs(namedOK = false, wildOK = false))
      if (in.token == LPAREN)
        p = liftApply(p, argumentPatterns())
      p
    }

    /** Patterns          ::=  Pattern [`,' Pattern]
     */
    def patterns() = commaSeparated(pattern)

    def patternsOpt(): List[Tree] =
      if (in.token == RPAREN) Nil else patterns()


    /** ArgumentPatterns  ::=  `(' [Patterns] `)'
     *                      |  `(' [Patterns `,'] Pattern2 `:' `_' `*' ')
     */
    def argumentPatterns(): List[Tree] =
      inParens(patternsOpt)

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    private def modOfToken(tok: Int, mods: Mods): Mods = tok match {
      case ABSTRACT  => mods.setAbstract
      case FINAL     => mods.setFinal
      case IMPLICIT  => mods.setImplicit
      case INLINE    => mods.setInline
      case LAZY      => mods.setLazy
      case OVERRIDE  => mods.setOverride
      case PRIVATE   => mods.setPrivate("")
      case PROTECTED => mods.setProtected("")
      case SEALED    => mods.setSealed
    }

    /** Drop `private' modifier when followed by a qualifier.
     *  Contract `abstract' and `override' to ABSOVERRIDE
     */
    /* private def normalize(mods: Mods): Mods =
      if ((mods is Private) && mods.hasPrivateWithin)
        normalize(mods &~ Private)
      else if (mods is AbstractAndOverride)
        normalize(addFlag(mods &~ (Abstract | Override), AbsOverride))
      else
        mods */

    private def addModifier(mods: Mods): Mods = {
      val tok = in.token
      in.skipToken()
      modOfToken(tok, mods)
    }

    /* private def compatible(flags1: FlagSet, flags2: FlagSet): Boolean = (
         flags1.isEmpty
      || flags2.isEmpty
      || flags1.isTermFlags && flags2.isTermFlags
      || flags1.isTypeFlags && flags2.isTypeFlags
    )

    def addFlag(mods: Mods, flag: FlagSet): Mods = {
      def incompatible(kind: String) = {
        syntaxError(s"modifier(s) `${mods.flags}' not allowed for $kind")
        Modifiers(flag)
      }
      if (compatible(mods.flags, flag)) mods | flag
      else flag match {
        case Trait => incompatible("trait")
        case Method => incompatible("method")
        case Mutable => incompatible("variable")
        case _ =>
          syntaxError(s"illegal modifier combination: ${mods.flags} and $flag")
          mods
      }
    } */

    /** Always add the syntactic `mod`, but check and conditionally add semantic `mod.flags`
     */
    /* def addMod(mods: Mods, mod: Mod): Mods =
      addFlag(mods, mod.flags).withAddedMod(mod) */

    /** AccessQualifier ::= "[" (id | this) "]"
     */
    def accessQualifierOpt(mods: Mods): Mods =
      if (in.token == LBRACKET) {
        if (!mods.privateWithin.isEmpty)
          syntaxError("duplicate private/protected qualifier")
        inBrackets {
          val within =
            if (in.token == THIS) { in.nextToken(); "this" }
            else ident()

          if (mods.isPrivate)
            mods.setPrivate(within)
          else
            mods.setProtected(within)
        }
      } else mods

    /** {Annotation} {Modifier}
     *  Modifiers      ::= {Modifier}
     *  LocalModifiers ::= {LocalModifier}
     *  AccessModifier ::= (private | protected) [AccessQualifier]
     *  Modifier       ::= LocalModifier
     *                  |  AccessModifier
     *                  |  override
     *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
     */
    def modifiers(allowed: BitSet = modifierTokens, start: Mods = emptyMods): Mods = {
      @tailrec
      def loop(mods: Mods): Mods = {
        if (allowed contains in.token) {
          val isAccessMod = accessModifierTokens contains in.token
          val mods1 = addModifier(mods)
          loop(if (isAccessMod) accessQualifierOpt(mods1) else mods1)
        } else if (in.token == NEWLINE && mods.hasAnnotations) {
          in.nextToken()
          loop(mods)
        } else {
          mods
        }
      }
      loop(start)
    }

    def implicitMods(): Mods = {
      accept(IMPLICIT)
      emptyMods.setImplicit
    }

    /** Wrap annotation or constructor in New(...).<init> */
    // def wrapNew(tpt: Tree) = Select(New(tpt), nme.CONSTRUCTOR)

    /** Adjust start of annotation or constructor to position of preceding @ or new */
    /* def adjustStart(start: Offset)(tree: Tree): Tree = {
      val tree1 = tree match {
        case Apply(fn, args) => cpy.Apply(tree)(adjustStart(start)(fn), args)
        case Select(qual, name) => cpy.Select(tree)(adjustStart(start)(qual), name)
        case _ => tree
      }
      if (tree1.pos.exists && start < tree1.pos.start)
        tree1.withPos(tree1.pos.withStart(start))
      else tree1
    } */

    /** Annotation        ::=  `@' SimpleType {ParArgumentExprs}
     */
    def annot() = {
      accept(AT)
      // if (in.token == INLINE) in.token = BACKQUOTED_IDENT // allow for now
      parArgumentExprss(simpleType())
    }

    def annotations(skipNewLines: Boolean = false): List[Tree] = {
      if (skipNewLines) newLineOptWhenFollowedBy(AT)
      if (in.token == AT) annot() :: annotations(skipNewLines)
      else Nil
    }

    def annotsAsMods(skipNewLines: Boolean = false): Mods =
      annotations(skipNewLines).foldRight(emptyMods) { (annot, mods) =>
        mods.withAddedAnnotation(annot)
      }

    def defAnnotsMods(allowed: BitSet): Mods =
      modifiers(allowed, annotsAsMods(skipNewLines = true))

 /* -------- PARAMETERS ------------------------------------------- */

    /** ClsTypeParamClause::=  `[' ClsTypeParam {`,' ClsTypeParam} `]'
     *  ClsTypeParam      ::=  {Annotation} [`+' | `-']
     *                         id [HkTypeParamClause] TypeParamBounds
     *
     *  DefTypeParamClause::=  `[' DefTypeParam {`,' DefTypeParam} `]'
     *  DefTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeParamBounds
     *
     *  TypTypeParamCaluse::=  `[' TypTypeParam {`,' TypTypeParam} `]'
     *  TypTypeParam      ::=  {Annotation} id [HkTypePamClause] TypeBounds
     *
     *  HkTypeParamClause ::=  `[' HkTypeParam {`,' HkTypeParam} `]'
     *  HkTypeParam       ::=  {Annotation} ['+' | `-'] (id [HkTypePamClause] | _') TypeBounds
     */
    def typeParamClause(ownerKind: ParamOwner.Value): List[Tree] = inBrackets {
      def typeParam(): Tree = {
        val isConcreteOwner = ownerKind == ParamOwner.Class || ownerKind == ParamOwner.Def
        val mods = {
          val mods1 = annotsAsMods()

          if (ownerKind != ParamOwner.Def)
            if (isIdent(raw.PLUS)) { in.nextToken(); mods1.setCovariant }
            else if (isIdent(raw.MINUS)) { in.nextToken(); mods1.setContravariant }
            else mods1
          else mods1
        }

        val name =
          if (isConcreteOwner || in.token != USCORE) ident()
          else {
            in.nextToken()
            fresh("_$")
          }

        val hkparams = typeParamClauseOpt(ParamOwner.TypeParam)

        if (isConcreteOwner) {
          val bounds = typeBounds()
          val cbs = contextBounds(name)
          liftTypeParam(mods, name, hkparams, bounds, cbs)
        }
        else {
          val bounds = typeBounds()
          liftTypeParam(mods, name, hkparams, bounds, Nil)
        }
      }
      commaSeparated(typeParam)
    }

    def typeParamClauseOpt(ownerKind: ParamOwner.Value): List[Tree] =
      if (in.token == LBRACKET) typeParamClause(ownerKind) else Nil

    /** ClsParamClauses   ::=  {ClsParamClause} [[nl] `(' `implicit' ClsParams `)']
     *  ClsParamClause    ::=  [nl] `(' [ClsParams] ')'
     *  ClsParams         ::=  ClsParam {`' ClsParam}
     *  ClsParam          ::=  {Annotation} [{Modifier} (`val' | `var') | `inline'] Param
     *  DefParamClauses   ::=  {DefParamClause} [[nl] `(' `implicit' DefParams `)']
     *  DefParamClause    ::=  [nl] `(' [DefParams] ')'
     *  DefParams         ::=  DefParam {`,' DefParam}
     *  DefParam          ::=  {Annotation} [`inline'] Param
     *  Param             ::=  id `:' ParamType [`=' Expr]
     */
    def paramClauses(owner: String, isType: Boolean, ofCaseClass: Boolean): List[List[Tree]] = {
      var isImplicit = false // use once
      var firstClauseOfCaseClass = ofCaseClass
      def param(): Tree = {
        var mods = annotsAsMods()
        if (isType) {
          mods = modifiers(start = mods)
          mods =
            if (in.token == VAL) {
              in.skipToken()
              mods.setValParam
            } else if (in.token == VAR) {
              in.skipToken()
              mods.setVarParam
            } else mods
        }
        else {
          if (in.token == INLINE) mods = addModifier(mods)
        }

        val name = ident()
        val tpt =
          if (!isType && in.token != COLON) {
            null
          } else {
            accept(COLON)
            paramType()
          }
        val default =
          if (in.token == EQUALS) { in.nextToken(); expr() }
          else null

        if (isImplicit) mods = mods.setImplicit
        liftParam(mods, name, tpt, default)
      }
      def paramClause(): List[Tree] = inParens {
        if (in.token == RPAREN) Nil
        else {
          if (in.token == IMPLICIT) isImplicit = true
          commaSeparated(param)
        }
      }
      def clauses(): List[List[Tree]] = {
        newLineOptWhenFollowedBy(LPAREN)
        if (in.token == LPAREN)
          paramClause() :: {
            firstClauseOfCaseClass = false
            if (isImplicit) Nil else clauses()
          }
        else Nil
      }
      val result = clauses()
      /* if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods is Implicit)))) {
        in.token match {
          case LBRACKET   => syntaxError("no type parameters allowed here")
          case EOF        => incompleteInputError(AuxConstructorNeedsNonImplicitParameter())
          case _          => syntaxError(AuxConstructorNeedsNonImplicitParameter(), start)
        }
      }
      val listOfErrors = checkVarArgsRules(result)
      listOfErrors.foreach { vparam =>
        syntaxError(VarArgsParamMustComeLast(), vparam.tpt.pos)
      } */
      result
    }

/* -------- DEFS ------------------------------------------- */

    /** Import  ::= import ImportExpr {`,' ImportExpr}
     */
    def importClause(): Tree = {
      val offset = accept(IMPORT)
      val items = commaSeparated[Tree](importExpr)
      liftImport(items)
    }

    /**  ImportExpr ::= StableId `.' (id | `_' | ImportSelectors)
     */
    val importExpr = () => unlift(path(thisOK = false, handleImport)) match {
      case Select(qual, name) =>
        liftImportItem(lift(qual), liftImportName(name) :: Nil)
      case t =>
        lift(t)
    }

    val handleImport = { tree: Tree =>
      if (in.token == USCORE) liftImportItem(tree, importSelector() :: Nil)
      else if (in.token == LBRACE) liftImportItem(tree, inBraces(importSelectors()))
      else tree
    }

    /** ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
     */
    def importSelectors(): List[Tree] =
      if (in.token == RBRACE) Nil
      else {
        val sel = importSelector()
        sel :: {
          if (in.token == COMMA) { // TODO: no selectors after `_`
            in.nextToken()
            importSelectors()
          }
          else Nil
        }
      }

   /** ImportSelector ::= id [`=>' id | `=>' `_']
     */
    def importSelector(): Tree = {
      if (in.token == USCORE) liftImportName("_")
      else {
        val from = ident()
        if (in.token == ARROW) {
          in.skipToken()
          if (in.token == USCORE) liftImportHide(from)
          else liftImportRename(from, ident())
        }
        else liftImportName(from)
      }
    }

    def posMods(start: Int, mods: Mods) = {
      in.nextToken()
      mods
    }

    /** Def      ::= val PatDef
     *             | var VarDef
     *             | def DefDef
     *             | type {nl} TypeDcl
     *             | TmplDef
     *  Dcl      ::= val ValDcl
     *             | var ValDcl
     *             | def DefDcl
     *             | type {nl} TypeDcl
     *  EnumCase ::= `case' (EnumClassDef | ObjectDef)
     */
    def defOrDcl(start: Int, mods: Mods): Tree = in.token match {
      case VAL =>
        in.skipToken()
        patDefOrDcl(start, mods)
      case VAR =>
        in.skipToken()
        patDefOrDcl(start, mods.setMutable)
      case DEF =>
        in.skipToken()
        defDefOrDcl(start, mods)
      case TYPE =>
        in.skipToken()
        typeDefOrDcl(start, mods)
      case CASE =>
        // enumCase(start, mods)
        syntaxError("enums not supported yet")
        null
      case _ =>
        tmplDef(start, mods)
    }

    /** PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  VarDef ::= PatDef | id {`,' id} `:' Type `=' `_'
     *  ValDcl ::= id {`,' id} `:' Type
     *  VarDcl ::= id {`,' id} `:' Type
     */
    def patDefOrDcl(start: Offset, mods: Mods): Tree = {
      val lhs = commaSeparated(pattern2)
      val allIdents = lhs.map(unlift) forall {
        case Ident(_) => true
        case _ => false
      }

      lazy val allNames = lhs.map(unlift).map {
        case Ident(name) => name
      }

      val tpt = typedOpt()
      val rhs =
        if (tpt == null || in.token == EQUALS) {
          accept(EQUALS)
          if (in.token == USCORE && tpt != null && mods.isMutable && allIdents) {
            wildcardIdent()
          } else {
            expr()
          }
        } else null

      lhs.map(unlift) match {
        case Ident(name) :: Nil =>
          if (rhs == null) liftValDecl(mods, name, tpt)
          else liftValDef(mods, name, tpt, rhs)
        case pat :: Nil =>
          liftPatDef(mods, pat, tpt, rhs)
        case _ =>
          if (rhs == null && allIdents) liftSeqDecl(mods, allNames, tpt)
          else liftSeqDef(mods, lhs, tpt, rhs)
      }
    }

    /* private def checkVarArgsRules(vparamss: List[List[untpd.ValDef]]): List[untpd.ValDef] = {
      def isVarArgs(tpt: Trees.Tree[Untyped]): Boolean = tpt match {
        case PostfixOp(_, op) if op.name == nme.raw.STAR => true
        case _ => false
      }

      vparamss.flatMap { params =>
        if (params.nonEmpty) {
          params.init.filter(valDef => isVarArgs(valDef.tpt))
        } else List()
      }
    } */

    /** DefDef ::= DefSig (`:' Type [`=' Expr] | "=" Expr)
     *           | this ParamClause ParamClauses `=' ConstrExpr
     *  DefDcl ::= DefSig `:' Type
     *  DefSig ::= id [DefTypeParamClause] ParamClauses
     */
    def defDefOrDcl(start: Offset, mods: Mods): Tree = {
      if (in.token == THIS) {
        in.nextToken()
        val vparamss = paramClauses("<init>", isType = false, ofCaseClass = false)
        val rhs = {
          accept(EQUALS)
          constrExpr()
        }
        liftSecondaryCtor(mods, vparamss, rhs)
      } else {
        val name = ident()
        val tparams = typeParamClauseOpt(ParamOwner.Def)
        val vparamss = paramClauses(name, isType = false, ofCaseClass = false)
        var tpt = fromWithinReturnType(typedOpt())
        val rhs =
          if (in.token == EQUALS) {
            in.nextToken()
            expr
          }
          else if (tpt != null)
            null
          else {
            if (!isExprIntro) syntaxError("missing return type")
            accept(EQUALS)
            expr()
          }

        if (rhs == null)
          liftDefDecl(mods, name, tparams, vparamss, tpt)
        else
          liftDefDef(mods, name, tparams, vparamss, tpt, rhs)
      }
    }

    /** ConstrExpr      ::=  SelfInvocation
     *                    |  ConstrBlock
     */
    def constrExpr(): Tree =
      if (in.token == LBRACE) constrBlock()
      else selfInvocation()

    /** SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
     */
    def selfInvocation(): Tree = {
      accept(THIS)
      newLineOptWhenFollowedBy(LBRACE)
      argumentExprss(liftApply(liftThis(""), argumentExprs()))
    }

    /** ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
     */
    def constrBlock(): Tree = {
      in.skipToken()
      val stats = selfInvocation() :: {
        if (isStatSep) { in.nextToken(); blockStatSeq() }
        else Nil
      }
      accept(RBRACE)
      liftBlock(stats)
    }

    /** TypeDef ::= type id [TypeParamClause] `=' Type
     *  TypeDcl ::= type id [TypeParamClause] TypeBounds
     */
    def typeDefOrDcl(start: Offset, mods: Mods): Tree = {
      newLinesOpt()
      val name = ident()
      val tparams = typeParamClauseOpt(ParamOwner.Type)
      in.token match {
        case EQUALS =>
          in.nextToken()
          liftTypeAlias(mods, name, tparams, typ())
        case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF =>
          liftTypeDecl(mods, name, tparams, typeBounds())
        case _ =>
          syntaxErrorOrIncomplete("`=', `>:', or `<:' expected")
          null
      }
    }

    /** TmplDef ::=  ([`case' | `enum]'] ‘class’ | trait’) ClassDef
     *            |  [`case'] `object' ObjectDef
     *            |  `enum' EnumDef
     */
    def tmplDef(start: Int, mods: Mods): Tree = {
      in.token match {
        case TRAIT =>
          in.nextToken()
          classDef(start, mods, isTrait = true)
        case CLASS =>
          in.nextToken()
          classDef(start, mods, isTrait = false)
        case CASECLASS =>
          in.nextToken()
          classDef(start, mods.setCase, isTrait = false)
        case OBJECT =>
          in.nextToken()
          objectDef(start, mods)
        case CASEOBJECT =>
          in.nextToken()
          objectDef(start, mods.setCase)
        case ENUM =>
          syntaxError("enums not supported yet")
          null
        case _ =>
          syntaxErrorOrIncomplete("expected start of definition")
          null
      }
    }

    /** ClassDef ::= id ClassConstr TemplateOpt
     */
    def classDef(start: Offset, mods: Mods, isTrait: Boolean): Tree = {
      val name = ident()
      val tparams = typeParamClauseOpt(ParamOwner.Class)
      val cmods = constrModsOpt(name)
      val vparamss = paramClauses(name, isType = true, ofCaseClass = mods.isCase)
      val ctor = liftPrimaryCtor(cmods, vparamss)
      val (parents, self, stats) = templateOpt
      if (isTrait)
        liftTrait(mods, name, tparams, ctor, parents, self, stats)
      else
        liftClass(mods, name, tparams, ctor, parents, self, stats)
    }

    /** ConstrMods        ::=  AccessModifier
     *                      |  Annotation {Annotation} (AccessModifier | `this')
     */
    def constrModsOpt(owner: String): Mods = {
      val mods = modifiers(accessModifierTokens, annotsAsMods())
      if (mods.hasAnnotations) //  TODO: !mods.hasFlags
        if (in.token == THIS) in.nextToken()
        else syntaxError("expected for annotated primary constructor")
      mods
    }

    /** ObjectDef       ::= id TemplateOpt
     */
    def objectDef(start: Offset, mods: Mods): Tree = {
      val name = ident()
      val (parents, self, stats) = templateOpt
      liftObject(mods, name, parents, self, stats)
    }

    /**  id ClassConstr [`extends' [ConstrApps]]
     *   [nl] ‘{’ EnumCaseStats ‘}’
     */
    /* def enumDef(start: Offset, mods: Modifiers, enumMod: Mod): Thicket = {
      val point = nameStart
      val modName = ident()
      val clsName = modName.toTypeName
      val constr = classConstr(clsName)
      val parents =
        if (in.token == EXTENDS) {
          in.nextToken();
          newLineOptWhenFollowedBy(LBRACE)
          if (in.token == LBRACE) Nil else tokenSeparated(WITH, constrApp)
        }
        else Nil
      val clsDef = atPos(start, point) {
        TypeDef(clsName, Template(constr, parents, EmptyValDef, Nil))
          .withMods(addMod(mods, enumMod)).setComment(in.getDocComment(start))
      }
      newLineOptWhenFollowedBy(LBRACE)
      val modDef = atPos(in.offset) {
        val body = inBraces(enumCaseStats)
        ModuleDef(modName, Template(emptyConstructor, Nil, EmptyValDef, body))
          .withMods(mods)
      }
      Thicket(clsDef :: modDef :: Nil)
    } */

    /** EnumCaseStats = EnumCaseStat {semi EnumCaseStat */
    /* def enumCaseStats(): List[DefTree] = {
      val cases = new ListBuffer[DefTree] += enumCaseStat()
      while (in.token != RBRACE) {
        acceptStatSep()
        cases += enumCaseStat()
      }
      cases.toList
    } */

    /** EnumCaseStat = {Annotation [nl]} {Modifier} EnumCase */
    /* def enumCaseStat(): DefTree =
      enumCase(in.offset, defAnnotsMods(modifierTokens)) */

    /** EnumCase = `case' (EnumClassDef | ObjectDef) */
    /* def enumCase(start: Offset, mods: Modifiers): DefTree = {
      val mods1 = mods.withAddedMod(atPos(in.offset)(Mod.EnumCase())) | Case
      accept(CASE)
      atPos(start, nameStart) {
        val id = termIdent()
        if (in.token == LBRACKET || in.token == LPAREN)
          classDefRest(start, mods1, id.name.toTypeName)
        else if (in.token == COMMA) {
          in.nextToken()
          val ids = commaSeparated(termIdent)
          PatDef(mods1, id :: ids, TypeTree(), EmptyTree)
        }
        else
          objectDefRest(start, mods1, id.name.asTermName)
      }
    } */

/* -------- TEMPLATES ------------------------------------------- */

    /** ConstrApp         ::=  SimpleType {ParArgumentExprs}
     */
    val constrApp = () => {
      val p = path(thisOK = true, isType = false)
      val (qual, name) = unlift(p) match {
        case Ident(name) => (null, name)
        case Select(qual, name) => (lift(qual), name)
      }

      val tparams =
        if (in.token == LBRACKET) typeArgs(namedOK = true , wildOK = false)
        else Nil

      // TODO: handle annotation
      val annotation = if (in.token == AT) annot()

      def recur(acc: Seq[Seq[Tree]]): Seq[Seq[Tree]] =
        if (in.token == LPAREN) recur(parArgumentExprs() +: acc)
        else acc.reverse

      val argss = recur(Nil)

      liftInitCall(qual, name, tparams, argss)
    }

    /** Template          ::=  ConstrApps [TemplateBody] | TemplateBody
     *  ConstrApps        ::=  ConstrApp {`with' ConstrApp}
     *
     *  @return  a pair consisting of the template, and a boolean which indicates
     *           whether the template misses a body (i.e. no {...} part).
     */
    def template: (Seq[Tree], Tree, Seq[Tree]) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        val (self, stats) = templateBodyOpt
        (Nil, self, stats)
      }
      else {
        val parents = tokenSeparated(WITH, constrApp)
        newLineOptWhenFollowedBy(LBRACE)
        val (self, stats) = templateBodyOpt
        (parents, self, stats)
      }
    }

    /** TemplateOpt = [`extends' Template | TemplateBody]
     */
    def templateOpt: (Seq[Tree], Tree, Seq[Tree]) =
      if (in.token == EXTENDS) { in.nextToken(); template }
      else {
        newLineOptWhenFollowedBy(LBRACE)
        if (in.token == LBRACE) template
        else (Nil, null, Nil)
      }

    /** TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     */
    def templateBodyOpt: (Tree, Seq[Tree]) = {
      if (in.token == LBRACE) templateBody() else (null, Nil)
    }

    def templateBody(): (Tree, List[Tree]) = {
      val r = inDefScopeBraces { templateStatSeq() }
      if (in.token == WITH) {
        syntaxError("Early definitions not supported")
        null
      }
      r
    }

/* -------- STATSEQS ------------------------------------------- */

    /** Create a tree representing a packaging */
    /* def makePackaging(start: Int, pkg: Tree, stats: List[Tree]): PackageDef = pkg match {
      case x: RefTree => atPos(start, pointOffset(pkg))(PackageDef(x, stats))
    } */

    /** Packaging ::= package QualId [nl] `{' TopStatSeq `}'
     */
    /* def packaging(start: Int): Tree = {
      val pkg = qualId()
      newLineOptWhenFollowedBy(LBRACE)
      val stats = inDefScopeBraces(topStatSeq)
      makePackaging(start, pkg, stats)
    } */

    /** TopStatSeq ::= TopStat {semi TopStat}
     *  TopStat ::= Annotations Modifiers TmplDef
     *            | Packaging
     *            | package object objectDef
     *            | Import
     *            |
     */
    /* def topStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        setLastStatOffset()
        if (in.token == PACKAGE) {
          val start = in.skipToken()
          if (in.token == OBJECT)
            stats += objectDef(start, atPos(start, in.skipToken()) { Modifiers(Package) })
          else stats += packaging(start)
        }
        else if (in.token == IMPORT)
          stats ++= importClause()
        else if (in.token == AT || isTemplateIntro || isModifier)
          stats +++= tmplDef(in.offset, defAnnotsMods(modifierTokens))
        else if (!isStatSep) {
          if (in.token == CASE)
            syntaxErrorOrIncomplete("only `case class` or `case object` allowed")
          else
            syntaxErrorOrIncomplete("expected class or object definition")
          if (mustStartStat) // do parse all definitions even if they are probably local (i.e. a "}" has been forgotten)
            defOrDcl(in.offset, defAnnotsMods(modifierTokens))
        }
        acceptStatSepUnlessAtEnd()
      }
      stats.toList
    } */

    /** TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStat {semi TemplateStat}
     *  TemplateStat     ::= Import
     *                     | Annotations Modifiers Def
     *                     | Annotations Modifiers Dcl
     *                     | EnumCaseStat
     *                     | Expr1
     *                     |
     */
    def templateStatSeq(): (Tree, List[Tree]) = checkNoEscapingPlaceholders {
      var self: Tree = null
      val stats = new ListBuffer[Tree]
      if (isExprIntro) {
        val first = expr1()
        if (in.token == ARROW) {
          unlift(first) match {
            case Ascribe(This(""), tpt) =>
              self = liftSelf("this", lift(tpt))
            case Ascribe(Ident(name), tpt) =>
              self = liftSelf(name, lift(tpt))
            case Ident(name) =>
              self = liftSelf(name)
            case _ =>
              syntaxError("not a legal self type clause")
              null
          }
          in.nextToken()
        } else {
          stats += first
          acceptStatSepUnlessAtEnd()
        }
      }
      var exitOnError = false
      while (!isStatSeqEnd && !exitOnError) {
        setLastStatOffset()
        if (in.token == IMPORT)
          stats += importClause()
        else if (isExprIntro)
          stats += expr1()
        else if (isDefIntro(modifierTokensOrCase))
          stats += defOrDcl(in.offset, defAnnotsMods(modifierTokens))
        else if (!isStatSep) {
          exitOnError = mustStartStat
          syntaxErrorOrIncomplete("illegal start of definition")
        }
        acceptStatSepUnlessAtEnd()
      }
      (self, stats.toList)
    }

    /** RefineStatSeq    ::= RefineStat {semi RefineStat}
     *  RefineStat       ::= Dcl
     *                     |
     *  (in reality we admit Defs and filter them out afterwards)
     */
    def refineStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        if (isDclIntro) {
          stats += defOrDcl(in.offset, emptyMods)
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete(
            "illegal start of declaration" +
            (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
             else ""))
        }
        acceptStatSepUnlessAtEnd()
      }
      stats.toList
    }

    def localDef(start: Int, isImplicit: Boolean = false): Tree = {
      val mods = defAnnotsMods(localModifierTokens)
      val mods1 = if (isImplicit) mods.setImplicit else mods
      defOrDcl(start, mods1)
    }

    /** BlockStatSeq ::= { BlockStat semi } [ResultExpr]
     *  BlockStat    ::= Import
     *                 | Annotations [implicit] [lazy] Def
     *                 | Annotations LocalModifiers TmplDef
     *                 | Expr1
     *                 |
     */
    def blockStatSeq(): List[Tree] = checkNoEscapingPlaceholders {
      val stats = new ListBuffer[Tree]
      var exitOnError = false
      while (!isStatSeqEnd && in.token != CASE && !exitOnError) {
        setLastStatOffset()
        if (in.token == IMPORT)
          stats += importClause()
        else if (isExprIntro)
          stats += expr(Location.InBlock)
        else if (isDefIntro(localModifierTokens))
          if (in.token == IMPLICIT) {
            val start = in.offset
            val imods = implicitMods()
            if (isBindingIntro) stats += implicitClosure(start, Location.InBlock, imods)
            else stats += localDef(start, isImplicit = true)
          } else {
            stats += localDef(in.offset)
          }
        else if (!isStatSep && (in.token != CASE)) {
          exitOnError = mustStartStat
          val addendum = if (isModifier) " (no modifiers allowed here)" else ""
          syntaxErrorOrIncomplete("illegal start of statement" + addendum)
        }
        acceptStatSepUnlessAtEnd(CASE)
      }
      stats.toList
    }

    /** CompilationUnit ::= {package QualId semi} TopStatSeq
     */
    /* def compilationUnit(): Tree = checkNoEscapingPlaceholders {
      def topstats(): List[Tree] = {
        val ts = new ListBuffer[Tree]
        while (in.token == SEMI) in.nextToken()
        val start = in.offset
        if (in.token == PACKAGE) {
          in.nextToken()
          if (in.token == OBJECT) {
            ts += objectDef(start, atPos(start, in.skipToken()) { Modifiers(Package) })
            if (in.token != EOF) {
              acceptStatSep()
              ts ++= topStatSeq()
            }
          } else {
            val pkg = qualId()
            newLineOptWhenFollowedBy(LBRACE)
            if (in.token == EOF)
              ts += makePackaging(start, pkg, List())
            else if (in.token == LBRACE) {
              ts += inDefScopeBraces(makePackaging(start, pkg, topStatSeq()))
              acceptStatSepUnlessAtEnd()
              ts ++= topStatSeq()
            }
            else {
              acceptStatSep()
              ts += makePackaging(start, pkg, topstats())
            }
          }
        }
        else
          ts ++= topStatSeq()

        ts.toList
      }

      topstats() match {
        case List(stat @ PackageDef(_, _)) => stat
        case Nil => EmptyTree  // without this case we'd get package defs without positions
        case stats => PackageDef(Ident(nme.EMPTY_PACKAGE), stats)
      }
    } */
  }

}
