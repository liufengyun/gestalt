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
    def syntaxError(msg: => String): Unit =
      ???
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
    /* def convertToParams(tree: Tree): List[ValDef] = tree match {
      case Parens(t)  => convertToParam(t) :: Nil
      case Tuple(ts)  => ts map (convertToParam(_))
      case t          => convertToParam(t) :: Nil
    } */

    /** Convert tree to formal parameter
    */
    /* def convertToParam(tree: Tree, mods: Modifiers = emptyMods(), expected: String = "formal parameter"): ValDef = tree match {
      case Ident(name) =>
        makeParameter(name.asTermName, TypeTree(), mods) withPos tree.pos
      case Typed(Ident(name), tpt) =>
        makeParameter(name.asTermName, tpt, mods) withPos tree.pos
      case _ =>
        syntaxError(s"not a legal $expected")
        makeParameter(nme.ERROR, tree, mods)
    } */

    /** Convert (qual)ident to type identifier
     */
    /* def convertToTypeId(tree: Tree): Tree = tree match {
      case id @ Ident(name) =>
        cpy.Ident(id)(name.toTypeName)
      case id @ Select(qual, name) =>
        cpy.Select(id)(qual, name.toTypeName)
      case _ =>
        syntaxError(IdentifierExpected(tree.show), tree.pos)
        tree
    } */

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

      // if (in.token == SYMBOLLIT) atPos(in.skipToken()) { SymbolLit(in.strVal) }
      if (in.token == INTERPOLATIONID) interpolatedString(inPattern)
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
        syntaxError("Annotation not supported yet")
        // annotTypeRest(t, annots :+ annot())
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
    def typeBounds(): Tree =
      liftTypeBounds(bound(SUPERTYPE), bound(SUBTYPE))

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
    /* def equalsExpr(): Tree = {
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
    } */

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
    /* val exprInParens = () => expr(Location.InParens)

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
          else new WildcardFunction(placeholderParams.reverse, t)
        finally placeholderParams = saved

        val t = expr1(location)
        if (in.token == ARROW) {
          placeholderParams = Nil // don't interpret `_' to the left of `=>` as placeholder
          wrapPlaceholders(closureRest(start, location, convertToParams(t)))
        }
        // TODO: support placeholders
        //else if (isWildcard(t)) {
        //  placeholderParams = placeholderParams ::: saved
        //  t
        //}
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
        If(cond, thenp, elsep)
      case WHILE =>
        in.skipToken()
        val cond = condExpr(DO)
        newLinesOpt()
        val body = expr()
        WhileDo(cond, body)
      case DO =>
        in.skipToken()
        val body = expr()
        if (isStatSep) in.nextToken()
        accept(WHILE)
        val cond = expr()
        DoWhile(body, cond)
      case TRY =>
        val tryOffset = in.offset
        in.skipToken()
        val body = expr()
        val (handler, handlerStart) =
          if (in.token == CATCH) {
            val pos = in.offset
            in.nextToken()
            (expr(), pos)
          } else (EmptyTree, -1)

        handler match {
          case Block(Nil, EmptyTree) =>
            assert(handlerStart != -1)
            syntaxError(
              EmptyCatchBlock(body),
              Position(handlerStart, endOffset(handler))
            )
          case _ =>
        }

        val finalizer =
          if (in.token == FINALLY) { accept(FINALLY); expr() }
          else {
            if (handler.isEmpty) warning(
              EmptyCatchAndFinallyBlock(body),
              source atPos Position(tryOffset, endOffset(body))
            )
            EmptyTree
          }
        ParsedTry(body, handler, finalizer)
      case THROW =>
        in.skipToken()
        Throw(expr())
      case RETURN =>
        in.skipToken()
        Return(if (isExprIntro) expr() else EmptyTree, null)
      case FOR =>
        forExpr()
      case _ =>
        expr1Rest(postfixExpr(), location)
    }

    def expr1Rest(t: Tree, location: Location.Value) = in.token match {
      case EQUALS =>
         t match {
           case Ident(_) | Select(_, _) | Apply(_, _) =>
             in.skipToken()
             Assign(t, expr())
           case _ =>
             t
         }
      case COLON =>
        ascription(t, location)
      case MATCH =>
        in.skipToken()
        inBraces(Match(t, caseClauses()))
      case _ =>
        t
    }

    def ascription(t: Tree, location: Location.Value) = atPos(startOffset(t), in.skipToken()) {
      in.token match {
        case USCORE =>
          val uscoreStart = in.skipToken()
          if (isIdent(nme.raw.STAR)) {
            in.nextToken()
            if (in.token != RPAREN) syntaxError(SeqWildcardPatternPos(), uscoreStart)
            Typed(t, atPos(uscoreStart) { Ident(tpnme.WILDCARD_STAR) })
          } else {
            syntaxErrorOrIncomplete(IncorrectRepeatedParameterSyntax())
            t
          }
        case AT if location != Location.InPattern =>
          (t /: annotations())(Annotated)
        case _ =>
          val tpt = typeDependingOn(location)
          if (isWildcard(t) && location != Location.InPattern) {
            val vd :: rest = placeholderParams
            placeholderParams =
              cpy.ValDef(vd)(tpt = tpt).withPos(vd.pos union tpt.pos) :: rest
          }
          Typed(t, tpt)
      }
    } */

    /** FunParams         ::=  Bindings
     *                     |   id
     *                     |   `_'
     *  Bindings          ::=  `(' [Binding {`,' Binding}] `)'
     */
    /* def funParams(mods: Modifiers, location: Location.Value): List[Tree] =
      if (in.token == LPAREN)
        inParens(if (in.token == RPAREN) Nil else commaSeparated(() => binding(mods)))
      else {
        val start = in.offset
        val name = bindingName()
        val t =
          if (in.token == COLON && location == Location.InBlock) {
            if (false) // Don't error yet, as the alternative syntax "implicit (x: T) => ... "
                       // is not supported by Scala2.x
              migrationWarningOrError(s"This syntax is no longer supported; parameter needs to be enclosed in (...)")

            in.nextToken()
            val t = infixType()

            if (false && in.isScala2Mode) {
              patch(source, Position(start), "(")
              patch(source, Position(in.lastOffset), ")")
            }
            t
          }
          else TypeTree()
        (atPos(start) { makeParameter(name, t, mods) }) :: Nil
      }  */

    /**  Binding           ::= (id | `_') [`:' Type]
     */
    /* def binding(mods: Modifiers): Tree =
      atPos(in.offset) { makeParameter(bindingName(), typedOpt(), mods) }

    def bindingName(): TermName =
      if (in.token == USCORE) {
        in.nextToken()
        WildcardParamName.fresh()
      }
      else ident()  */

    /** Expr         ::= implicit id `=>' Expr
     *  BlockResult  ::= implicit id [`:' InfixType] `=>' Block // Scala2 only
     */
    /* def implicitClosure(start: Int, location: Location.Value, implicitMods: Modifiers): Tree =
      closureRest(start, location, funParams(implicitMods, location))

    def closureRest(start: Int, location: Location.Value, params: List[Tree]): Tree =
      atPos(start, in.offset) {
        accept(ARROW)
        Function(params, if (location == Location.InBlock) block() else expr())
      }  */

    /** PostfixExpr   ::= InfixExpr [id [nl]]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr id [nl] InfixExpr
     */
    // def postfixExpr(): Tree =
    //  infixOps(prefixExpr(), canStartExpressionTokens, prefixExpr, maybePostfix = true)

    /** PrefixExpr   ::= [`-' | `+' | `~' | `!'] SimpleExpr
    */
    /* val prefixExpr = () =>
      if (isIdent && nme.raw.isUnary(in.name)) {
        val start = in.offset
        val op = termIdent()
        if (op.name == nme.raw.MINUS && isNumericLit)
          simpleExprRest(literal(start), canApply = true)
        else
          atPos(start) { PrefixOp(op, simpleExpr()) }
      }
      else simpleExpr()  */

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
    /* def simpleExpr(): Tree = {
      var canApply = true
      val t = in.token match {
        case XMLSTART =>
          xmlLiteral()
        case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER =>
          path(thisOK = true)
        case USCORE =>
          val start = in.skipToken()
          val pname = WildcardParamName.fresh()
          val param = ValDef(pname, TypeTree(), EmptyTree).withFlags(SyntheticTermParam)
            .withPos(Position(start))
          placeholderParams = param :: placeholderParams
          atPos(start) { Ident(pname) }
        case LPAREN =>
          atPos(in.offset) { makeTupleOrParens(inParens(exprsInParensOpt())) }
        case LBRACE =>
          canApply = false
          blockExpr()
        case NEW =>
          canApply = false
          val start = in.skipToken()
          val (impl, missingBody) = template(emptyConstructor)
          impl.parents match {
            case parent :: Nil if missingBody =>
              if (parent.isType) ensureApplied(wrapNew(parent)) else parent
            case _ =>
              New(impl.withPos(Position(start, in.lastOffset)))
          }
        case _ =>
          if (isLiteral) literal()
          else {
            syntaxErrorOrIncomplete(IllegalStartSimpleExpr(tokenString(in.token)))
            errorTermTree
          }
      }
      simpleExprRest(t, canApply)
    }

    def simpleExprRest(t: Tree, canApply: Boolean = true): Tree = {
      if (canApply) newLineOptWhenFollowedBy(LBRACE)
      in.token match {
        case DOT =>
          in.nextToken()
          simpleExprRest(selector(t), canApply = true)
        case LBRACKET =>
          val tapp = atPos(startOffset(t), in.offset) { TypeApply(t, typeArgs(namedOK = true, wildOK = false)) }
          simpleExprRest(tapp, canApply = true)
        case LPAREN | LBRACE if canApply =>
          val app = atPos(startOffset(t), in.offset) { Apply(t, argumentExprs()) }
          simpleExprRest(app, canApply = true)
        case USCORE =>
          atPos(startOffset(t), in.skipToken()) { PostfixOp(t, Ident(nme.WILDCARD)) }
        case _ =>
          t
      }
    }  */

    /**   ExprsInParens     ::=  ExprInParens {`,' ExprInParens}
     */
    // def exprsInParensOpt(): List[Tree] =
    //  if (in.token == RPAREN) Nil else commaSeparated(exprInParens)

    /** ParArgumentExprs ::= `(' [ExprsInParens] `)'
     *                    |  `(' [ExprsInParens `,'] PostfixExpr `:' `_' `*' ')' \
     */
    // def parArgumentExprs(): List[Tree] =
    //  inParens(if (in.token == RPAREN) Nil else commaSeparated(argumentExpr))

    /** ArgumentExprs ::= ParArgumentExprs
     *                 |  [nl] BlockExpr
     */
    /* def argumentExprs(): List[Tree] =
      if (in.token == LBRACE) blockExpr() :: Nil else parArgumentExprs()

    val argumentExpr = () => exprInParens() match {
      case a @ Assign(Ident(id), rhs) => cpy.NamedArg(a)(id, rhs)
      case e => e
    } */

    /** ArgumentExprss ::= {ArgumentExprs}
     */
    /* def argumentExprss(fn: Tree): Tree = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LPAREN || in.token == LBRACE) argumentExprss(Apply(fn, argumentExprs()))
      else fn
    } */

    /** ParArgumentExprss ::= {ParArgumentExprs}
     */
    /* def parArgumentExprss(fn: Tree): Tree =
      if (in.token == LPAREN) parArgumentExprss(Apply(fn, parArgumentExprs()))
      else fn */

    /** BlockExpr ::= `{' (CaseClauses | Block) `}'
     */
    /* def blockExpr(): Tree = atPos(in.offset) {
      inDefScopeBraces {
        if (in.token == CASE) Match(EmptyTree, caseClauses())
        else block()
      }
    } */

    /** Block ::= BlockStatSeq
     *  @note  Return tree does not carry source position.
     */
    /* def block(): Tree = {
      val stats = blockStatSeq()
      def isExpr(stat: Tree) = !(stat.isDef || stat.isInstanceOf[Import])
      if (stats.nonEmpty && isExpr(stats.last)) Block(stats.init, stats.last)
      else Block(stats, EmptyTree)
    } */

    /** Guard ::= if PostfixExpr
     */
    /* def guard(): Tree =
      if (in.token == IF) { in.nextToken(); postfixExpr() }
      else EmptyTree  */

    /** Enumerators ::= Generator {semi Enumerator | Guard}
     */
    /* def enumerators(): List[Tree] = generator() :: enumeratorsRest()

    def enumeratorsRest(): List[Tree] =
      if (isStatSep) { in.nextToken(); enumerator() :: enumeratorsRest() }
      else if (in.token == IF) guard() :: enumeratorsRest()
      else Nil  */

    /** Enumerator  ::=  Generator
     *                |  Guard
     *                |  Pattern1 `=' Expr
     */
    /* def enumerator(): Tree =
      if (in.token == IF) guard()
      else {
        val pat = pattern1()
        if (in.token == EQUALS) atPos(startOffset(pat), in.skipToken()) { GenAlias(pat, expr()) }
        else generatorRest(pat)
      }  */

    /** Generator   ::=  Pattern `<-' Expr
     */
    /* def generator(): Tree = generatorRest(pattern1())

    def generatorRest(pat: Tree) =
      atPos(startOffset(pat), accept(LARROW)) { GenFrom(pat, expr()) } */

    /** ForExpr  ::= `for' (`(' Enumerators `)' | `{' Enumerators `}')
     *                {nl} [`yield'] Expr
     *            |  `for' Enumerators (`do' Expr | `yield' Expr)
     */
    /* def forExpr(): Tree = atPos(in.skipToken()) {
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
              atPos(lparenOffset) { makeTupleOrParens(pats) } // note: alternatives `|' need to be weeded out by typer.
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
      if (in.token == YIELD) { in.nextToken(); ForYield(enums, expr()) }
      else if (in.token == DO) { in.nextToken(); ForDo(enums, expr()) }
      else {
        if (!wrappedEnums) syntaxErrorOrIncomplete(YieldOrDoExpectedInForComprehension())
        ForDo(enums, expr())
      }
    }  */

    /** CaseClauses ::= CaseClause {CaseClause}
    */
    /* def caseClauses(): List[CaseDef] = {
      val buf = new ListBuffer[CaseDef]
      buf += caseClause()
      while (in.token == CASE) buf += caseClause()
      buf.toList
    } */

   /** CaseClause ::= case Pattern [Guard] `=>' Block
    */
    /* def caseClause(): CaseDef = atPos(in.offset) {
      accept(CASE)
      CaseDef(pattern(), guard(), atPos(accept(ARROW)) { block() })
    }  */

    /* -------- PATTERNS ------------------------------------------- */

    /**  Pattern           ::=  Pattern1 { `|' Pattern1 }
     */
    /* val pattern = () => {
      val pat = pattern1()
      if (isIdent(nme.raw.BAR))
        atPos(startOffset(pat)) { Alternative(pat :: patternAlts()) }
      else pat
    }

    def patternAlts(): List[Tree] =
      if (isIdent(nme.raw.BAR)) { in.nextToken(); pattern1() :: patternAlts() }
      else Nil  */

    /**  Pattern1          ::= PatVar Ascription
     *                      |  Pattern2
     */
    /* def pattern1(): Tree = {
      val p = pattern2()
      if (isVarPattern(p) && in.token == COLON) ascription(p, Location.InPattern)
      else p
    }  */

    /**  Pattern2    ::=  [varid `@'] InfixPattern
     */
    /* val pattern2 = () => infixPattern() match {
      case p @ Ident(name) if isVarPattern(p) && in.token == AT =>
        val offset = in.skipToken()

        // compatibility for Scala2 `x @ _*` syntax
        infixPattern() match {
          case pt @ Ident(tpnme.WILDCARD_STAR) =>
            migrationWarningOrError("The syntax `x @ _*' is no longer supported; use `x : _*' instead", startOffset(p))
            atPos(startOffset(p), offset) { Typed(p, pt) }
          case p =>
            atPos(startOffset(p), offset) { Bind(name, p) }
        }
      case p @ Ident(tpnme.WILDCARD_STAR) =>
        // compatibility for Scala2 `_*` syntax
        migrationWarningOrError("The syntax `_*' is no longer supported; use `x : _*' instead", startOffset(p))
        atPos(startOffset(p)) { Typed(Ident(nme.WILDCARD), p) }
      case p =>
        p
    } */

    /**  InfixPattern ::= SimplePattern {id [nl] SimplePattern}
     */
    // def infixPattern(): Tree =
    //  infixOps(simplePattern(), canStartExpressionTokens, simplePattern, notAnOperator = nme.raw.BAR)

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
    /* val simplePattern = () => in.token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
        path(thisOK = true) match {
          case id @ Ident(nme.raw.MINUS) if isNumericLit => literal(startOffset(id))
          case t => simplePatternRest(t)
        }
      case USCORE =>
        val wildIndent = wildcardIdent()

        // compatibility for Scala2 `x @ _*` and `_*` syntax
        // `x: _*' is parsed in `ascription'
        if (isIdent(nme.raw.STAR)) {
          in.nextToken()
          if (in.token != RPAREN) syntaxError(SeqWildcardPatternPos(), wildIndent.pos)
          atPos(wildIndent.pos) { Ident(tpnme.WILDCARD_STAR) }
        } else wildIndent
      case LPAREN =>
        atPos(in.offset) { makeTupleOrParens(inParens(patternsOpt())) }
      case LBRACE =>
        dotSelectors(blockExpr())
      case XMLSTART =>
        xmlLiteralPattern()
      case _ =>
        if (isLiteral) literal(inPattern = true)
        else {
          syntaxErrorOrIncomplete(IllegalStartOfSimplePattern())
          errorTermTree
        }
    }

    def simplePatternRest(t: Tree): Tree = {
      var p = t
      if (in.token == LBRACKET)
        p = atPos(startOffset(t), in.offset) { TypeApply(p, typeArgs(namedOK = false, wildOK = false)) }
      if (in.token == LPAREN)
        p = atPos(startOffset(t), in.offset) { Apply(p, argumentPatterns()) }
      p
    }  */

    /** Patterns          ::=  Pattern [`,' Pattern]
     */
    /* def patterns() = commaSeparated(pattern)

    def patternsOpt(): List[Tree] =
      if (in.token == RPAREN) Nil else patterns()  */


    /** ArgumentPatterns  ::=  `(' [Patterns] `)'
     *                      |  `(' [Patterns `,'] Pattern2 `:' `_' `*' ')
     */
    /* def argumentPatterns(): List[Tree] =
      inParens(patternsOpt) */

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    /* private def modOfToken(tok: Int): Mod = tok match {
      case ABSTRACT  => Mod.Abstract()
      case FINAL     => Mod.Final()
      case IMPLICIT  => Mod.Implicit()
      case INLINE    => Mod.Inline()
      case LAZY      => Mod.Lazy()
      case OVERRIDE  => Mod.Override()
      case PRIVATE   => Mod.Private()
      case PROTECTED => Mod.Protected()
      case SEALED    => Mod.Sealed()
    } */

    /** Drop `private' modifier when followed by a qualifier.
     *  Contract `abstract' and `override' to ABSOVERRIDE
     */
    /* private def normalize(mods: Modifiers): Modifiers =
      if ((mods is Private) && mods.hasPrivateWithin)
        normalize(mods &~ Private)
      else if (mods is AbstractAndOverride)
        normalize(addFlag(mods &~ (Abstract | Override), AbsOverride))
      else
        mods

    private def addModifier(mods: Modifiers): Modifiers = {
      val tok = in.token
      val mod = atPos(in.skipToken()) { modOfToken(tok) }

      if (mods is mod.flags) syntaxError(RepeatedModifier(mod.flags.toString))
      addMod(mods, mod)
    }

    private def compatible(flags1: FlagSet, flags2: FlagSet): Boolean = (
         flags1.isEmpty
      || flags2.isEmpty
      || flags1.isTermFlags && flags2.isTermFlags
      || flags1.isTypeFlags && flags2.isTypeFlags
    )

    def addFlag(mods: Modifiers, flag: FlagSet): Modifiers = {
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
    /* def addMod(mods: Modifiers, mod: Mod): Modifiers =
      addFlag(mods, mod.flags).withAddedMod(mod) */

    /** AccessQualifier ::= "[" (id | this) "]"
     */
    /* def accessQualifierOpt(mods: Modifiers): Modifiers =
      if (in.token == LBRACKET) {
        if ((mods is Local) || mods.hasPrivateWithin)
          syntaxError("duplicate private/protected qualifier")
        inBrackets {
          if (in.token == THIS) { in.nextToken(); mods | Local }
          else mods.withPrivateWithin(ident().toTypeName)
        }
      } else mods */

    /** {Annotation} {Modifier}
     *  Modifiers      ::= {Modifier}
     *  LocalModifiers ::= {LocalModifier}
     *  AccessModifier ::= (private | protected) [AccessQualifier]
     *  Modifier       ::= LocalModifier
     *                  |  AccessModifier
     *                  |  override
     *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
     */
    /* def modifiers(allowed: BitSet = modifierTokens, start: Modifiers = Modifiers()): Modifiers = {
      @tailrec
      def loop(mods: Modifiers): Modifiers = {
        if (allowed contains in.token) {
          val isAccessMod = accessModifierTokens contains in.token
          val mods1 = addModifier(mods)
          loop(if (isAccessMod) accessQualifierOpt(mods1) else mods1)
        } else if (in.token == NEWLINE && (mods.hasFlags || mods.hasAnnotations)) {
          in.nextToken()
          loop(mods)
        } else {
          mods
        }
      }
      normalize(loop(start))
    }

    def implicitMods(): Modifiers =
      addMod(EmptyModifiers, atPos(accept(IMPLICIT)) { Mod.Implicit() }) */

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
    /* def annot() =
      adjustStart(accept(AT)) {
        if (in.token == INLINE) in.token = BACKQUOTED_IDENT // allow for now
        ensureApplied(parArgumentExprss(wrapNew(simpleType())))
      }

    def annotations(skipNewLines: Boolean = false): List[Tree] = {
      if (skipNewLines) newLineOptWhenFollowedBy(AT)
      if (in.token == AT) annot() :: annotations(skipNewLines)
      else Nil
    }

    def annotsAsMods(skipNewLines: Boolean = false): Modifiers =
      Modifiers() withAnnotations annotations(skipNewLines)

    def defAnnotsMods(allowed: BitSet): Modifiers =
      modifiers(allowed, annotsAsMods(skipNewLines = true))  */

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
    /* def typeParamClause(ownerKind: ParamOwner.Value): List[TypeDef] = inBrackets {
      def typeParam(): TypeDef = {
        val isConcreteOwner = ownerKind == ParamOwner.Class || ownerKind == ParamOwner.Def
        val start = in.offset
        val mods = atPos(start) {
          annotsAsMods() | {
            if (ownerKind == ParamOwner.Class) Param | PrivateLocal
            else Param
          } | {
            if (ownerKind != ParamOwner.Def)
              if (isIdent(nme.raw.PLUS)) { in.nextToken(); Covariant }
              else if (isIdent(nme.raw.MINUS)) { in.nextToken(); Contravariant }
              else EmptyFlags
            else EmptyFlags
          }
        }
        atPos(start, nameStart) {
          val name =
            if (isConcreteOwner || in.token != USCORE) ident().toTypeName
            else {
              in.nextToken()
              WildcardParamName.fresh().toTypeName
            }
          val hkparams = typeParamClauseOpt(ParamOwner.TypeParam)
          val bounds =
            if (isConcreteOwner) typeParamBounds(name)
            else typeBounds()
          TypeDef(name, lambdaAbstract(hkparams, bounds)).withMods(mods)
        }
      }
      commaSeparated(typeParam)
    }

    def typeParamClauseOpt(ownerKind: ParamOwner.Value): List[TypeDef] =
      if (in.token == LBRACKET) typeParamClause(ownerKind) else Nil  */

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
    /* def paramClauses(owner: Name, ofCaseClass: Boolean = false): List[List[ValDef]] = {
      var imods: Modifiers = EmptyModifiers
      var implicitOffset = -1 // use once
      var firstClauseOfCaseClass = ofCaseClass
      def param(): ValDef = {
        val start = in.offset
        var mods = annotsAsMods()
        if (owner.isTypeName) {
          mods = modifiers(start = mods) | ParamAccessor
          mods =
            atPos(start, in.offset) {
              if (in.token == VAL) {
                val mod = atPos(in.skipToken()) { Mod.Val() }
                mods.withAddedMod(mod)
              } else if (in.token == VAR) {
                val mod = atPos(in.skipToken()) { Mod.Var() }
                addMod(mods, mod)
              } else {
                if (!(mods.flags &~ (ParamAccessor | Inline)).isEmpty)
                  syntaxError("`val' or `var' expected")
                if (firstClauseOfCaseClass) mods else mods | PrivateLocal
              }
            }
        }
        else {
          if (in.token == INLINE) mods = addModifier(mods)
          mods = atPos(start) { mods | Param }
        }
        atPos(start, nameStart) {
          val name = ident()
          val tpt =
            if (owner.isTermName && in.token != COLON) {
              TypeTree()  // XX-METHOD-INFER
            } else {
              accept(COLON)
              if (in.token == ARROW && owner.isTypeName && !(mods is Local))
                syntaxError(s"${if (mods is Mutable) "`var'" else "`val'"} parameters may not be call-by-name")
              paramType()
            }
          val default =
            if (in.token == EQUALS) { in.nextToken(); expr() }
            else EmptyTree
          if (implicitOffset >= 0) {
            mods = mods.withPos(mods.pos.union(Position(implicitOffset, implicitOffset)))
            implicitOffset = -1
          }
          for (imod <- imods.mods) mods = addMod(mods, imod)
          ValDef(name, tpt, default).withMods(mods)
        }
      }
      def paramClause(): List[ValDef] = inParens {
        if (in.token == RPAREN) Nil
        else {
          if (in.token == IMPLICIT) {
            implicitOffset = in.offset
            imods = implicitMods()
          }
          commaSeparated(param)
        }
      }
      def clauses(): List[List[ValDef]] = {
        newLineOptWhenFollowedBy(LPAREN)
        if (in.token == LPAREN)
          paramClause() :: {
            firstClauseOfCaseClass = false
            if (imods.hasFlags) Nil else clauses()
          }
        else Nil
      }
      val start = in.offset
      val result = clauses()
      if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods is Implicit)))) {
        in.token match {
          case LBRACKET   => syntaxError("no type parameters allowed here")
          case EOF        => incompleteInputError(AuxConstructorNeedsNonImplicitParameter())
          case _          => syntaxError(AuxConstructorNeedsNonImplicitParameter(), start)
        }
      }
      val listOfErrors = checkVarArgsRules(result)
      listOfErrors.foreach { vparam =>
        syntaxError(VarArgsParamMustComeLast(), vparam.tpt.pos)
      }
      result
    } */

/* -------- DEFS ------------------------------------------- */

    /** Import  ::= import ImportExpr {`,' ImportExpr}
     */
    /* def importClause(): List[Tree] = {
      val offset = accept(IMPORT)
      commaSeparated(importExpr) match {
        case t :: rest =>
          // The first import should start at the position of the keyword.
          t.withPos(t.pos.withStart(offset)) :: rest
        case nil => nil
      }
    } */

    /**  ImportExpr ::= StableId `.' (id | `_' | ImportSelectors)
     */
    /* val importExpr = () => path(thisOK = false, handleImport) match {
      case imp: Import =>
        imp
      case sel @ Select(qual, name) =>
        val selector = atPos(pointOffset(sel)) { Ident(name) }
        cpy.Import(sel)(qual, selector :: Nil)
      case t =>
        accept(DOT)
        Import(t, Ident(nme.WILDCARD) :: Nil)
    }

    val handleImport = { tree: Tree =>
      if (in.token == USCORE) Import(tree, importSelector() :: Nil)
      else if (in.token == LBRACE) Import(tree, inBraces(importSelectors()))
      else tree
    } */

    /** ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
     */
    /* def importSelectors(): List[Tree] =
      if (in.token == RBRACE) Nil
      else {
        val sel = importSelector()
        sel :: {
          if (!isWildcardArg(sel) && in.token == COMMA) {
            in.nextToken()
            importSelectors()
          }
          else Nil
        }
      }

   /** ImportSelector ::= id [`=>' id | `=>' `_']
     */
    def importSelector(): Tree = {
      val from = termIdentOrWildcard()
      if (from.name != nme.WILDCARD && in.token == ARROW)
        atPos(startOffset(from), in.skipToken()) {
          Thicket(from, termIdentOrWildcard())
        }
      else from
    }

    def posMods(start: Int, mods: Modifiers) = {
      val mods1 = atPos(start)(mods)
      in.nextToken()
      mods1
    } */

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
    /* def defOrDcl(start: Int, mods: Modifiers): Tree = in.token match {
      case VAL =>
        val mod = atPos(in.skipToken()) { Mod.Val() }
        val mods1 = mods.withAddedMod(mod)
        patDefOrDcl(start, mods1)
      case VAR =>
        val mod = atPos(in.skipToken()) { Mod.Var() }
        val mod1 = addMod(mods, mod)
        patDefOrDcl(start, mod1)
      case DEF =>
        defDefOrDcl(start, posMods(start, mods))
      case TYPE =>
        typeDefOrDcl(start, posMods(start, mods))
      case CASE =>
        enumCase(start, mods)
      case _ =>
        tmplDef(start, mods)
    } */

    /** PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  VarDef ::= PatDef | id {`,' id} `:' Type `=' `_'
     *  ValDcl ::= id {`,' id} `:' Type
     *  VarDcl ::= id {`,' id} `:' Type
     */
    /* def patDefOrDcl(start: Offset, mods: Modifiers): Tree = atPos(start, nameStart) {
      val lhs = commaSeparated(pattern2)
      val tpt = typedOpt()
      val rhs =
        if (tpt.isEmpty || in.token == EQUALS) {
          accept(EQUALS)
          if (in.token == USCORE && !tpt.isEmpty && (mods is Mutable) &&
              (lhs.toList forall (_.isInstanceOf[Ident]))) {
            wildcardIdent()
          } else {
            expr()
          }
        } else EmptyTree
      lhs match {
        case (id @ Ident(name: TermName)) :: Nil => {
          ValDef(name, tpt, rhs).withMods(mods).setComment(in.getDocComment(start))
        } case _ =>
          PatDef(mods, lhs, tpt, rhs)
      }
    }



    private def checkVarArgsRules(vparamss: List[List[untpd.ValDef]]): List[untpd.ValDef] = {
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
    /* def defDefOrDcl(start: Offset, mods: Modifiers): Tree = atPos(start, nameStart) {
      def scala2ProcedureSyntax(resultTypeStr: String) = {
        val toInsert =
          if (in.token == LBRACE) s"$resultTypeStr ="
          else ": Unit "  // trailing space ensures that `def f()def g()` works.
        in.testScala2Mode(s"Procedure syntax no longer supported; `$toInsert' should be inserted here") && {
          patch(source, Position(in.lastOffset), toInsert)
          true
        }
      }
      if (in.token == THIS) {
        in.nextToken()
        val vparamss = paramClauses(nme.CONSTRUCTOR)
        if (in.isScala2Mode) newLineOptWhenFollowedBy(LBRACE)
        val rhs = {
          if (!(in.token == LBRACE && scala2ProcedureSyntax(""))) accept(EQUALS)
          atPos(in.offset) { constrExpr() }
        }
        makeConstructor(Nil, vparamss, rhs).withMods(mods)
      } else {
        val mods1 = addFlag(mods, Method)
        val name = ident()
        val tparams = typeParamClauseOpt(ParamOwner.Def)
        val vparamss = paramClauses(name)
        var tpt = fromWithinReturnType(typedOpt())
        if (in.isScala2Mode) newLineOptWhenFollowedBy(LBRACE)
        val rhs =
          if (in.token == EQUALS) {
            in.nextToken()
            expr
          }
          else if (!tpt.isEmpty)
            EmptyTree
          else if (scala2ProcedureSyntax(": Unit")) {
            tpt = scalaUnit
            if (in.token == LBRACE) expr()
            else EmptyTree
          }
          else {
            if (!isExprIntro) syntaxError(MissingReturnType(), in.lastOffset)
            accept(EQUALS)
            expr()
          }
        DefDef(name, tparams, vparamss, tpt, rhs).withMods(mods1).setComment(in.getDocComment(start))
      }
    } */

    /** ConstrExpr      ::=  SelfInvocation
     *                    |  ConstrBlock
     */
    /* def constrExpr(): Tree =
      if (in.token == LBRACE) constrBlock()
      else Block(selfInvocation() :: Nil, Literal(Constant(()))) */

    /** SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
     */
    /* def selfInvocation(): Tree =
      atPos(accept(THIS)) {
        newLineOptWhenFollowedBy(LBRACE)
        argumentExprss(Apply(Ident(nme.CONSTRUCTOR), argumentExprs()))
      } */

    /** ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
     */
    /* def constrBlock(): Tree =
      atPos(in.skipToken()) {
        val stats = selfInvocation() :: {
          if (isStatSep) { in.nextToken(); blockStatSeq() }
          else Nil
        }
        accept(RBRACE)
        Block(stats, Literal(Constant(())))
      } */

    /** TypeDef ::= type id [TypeParamClause] `=' Type
     *  TypeDcl ::= type id [TypeParamClause] TypeBounds
     */
    /* def typeDefOrDcl(start: Offset, mods: Modifiers): Tree = {
      newLinesOpt()
      atPos(start, nameStart) {
        val name = ident().toTypeName
        val tparams = typeParamClauseOpt(ParamOwner.Type)
        in.token match {
          case EQUALS =>
            in.nextToken()
            TypeDef(name, lambdaAbstract(tparams, typ())).withMods(mods).setComment(in.getDocComment(start))
          case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF =>
            TypeDef(name, lambdaAbstract(tparams, typeBounds())).withMods(mods).setComment(in.getDocComment(start))
          case _ =>
            syntaxErrorOrIncomplete("`=', `>:', or `<:' expected")
            EmptyTree
        }
      }
    } */

    /** TmplDef ::=  ([`case' | `enum]'] ‘class’ | trait’) ClassDef
     *            |  [`case'] `object' ObjectDef
     *            |  `enum' EnumDef
     */
    /* def tmplDef(start: Int, mods: Modifiers): Tree = {
      in.token match {
        case TRAIT =>
          classDef(start, posMods(start, addFlag(mods, Trait)))
        case CLASS =>
          classDef(start, posMods(start, mods))
        case CASECLASS =>
          classDef(start, posMods(start, mods | Case))
        case OBJECT =>
          objectDef(start, posMods(start, mods | Module))
        case CASEOBJECT =>
          objectDef(start, posMods(start, mods | Case | Module))
        case ENUM =>
          val enumMod = atPos(in.skipToken()) { Mod.Enum() }
          if (in.token == CLASS) tmplDef(start, addMod(mods, enumMod))
          else enumDef(start, mods, enumMod)
        case _ =>
          syntaxErrorOrIncomplete("expected start of definition")
          EmptyTree
      }
    } */

    /** ClassDef ::= id ClassConstr TemplateOpt
     */
    /* def classDef(start: Offset, mods: Modifiers): TypeDef = atPos(start, nameStart) {
      classDefRest(start, mods, ident().toTypeName)
    }

    def classDefRest(start: Offset, mods: Modifiers, name: TypeName): TypeDef = {
      val constr = classConstr(name, isCaseClass = mods is Case)
      val templ = templateOpt(constr)
      TypeDef(name, templ).withMods(mods).setComment(in.getDocComment(start))
    } */

    /** ClassConstr ::= [ClsTypeParamClause] [ConstrMods] ClsParamClauses
     */
    /* def classConstr(owner: Name, isCaseClass: Boolean = false): DefDef = atPos(in.lastOffset) {
      val tparams = typeParamClauseOpt(ParamOwner.Class)
      val cmods = constrModsOpt(owner)
      val vparamss = paramClauses(owner, isCaseClass)
      makeConstructor(tparams, vparamss).withMods(cmods)
    } */

    /** ConstrMods        ::=  AccessModifier
     *                      |  Annotation {Annotation} (AccessModifier | `this')
     */
    /* def constrModsOpt(owner: Name): Modifiers = {
      val mods = modifiers(accessModifierTokens, annotsAsMods())
      if (mods.hasAnnotations && !mods.hasFlags)
        if (in.token == THIS) in.nextToken()
        else syntaxError(AnnotatedPrimaryConstructorRequiresModifierOrThis(owner), mods.annotations.last.pos)
      mods
    } */

    /** ObjectDef       ::= id TemplateOpt
     */
    /* def objectDef(start: Offset, mods: Modifiers): ModuleDef = atPos(start, nameStart) {
      objectDefRest(start, mods, ident())
    }

    def objectDefRest(start: Offset, mods: Modifiers, name: TermName): ModuleDef = {
      val template = templateOpt(emptyConstructor)
      ModuleDef(name, template).withMods(mods).setComment(in.getDocComment(start))
    } */

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
    /* val constrApp = () => {
      val t = annotType()
      if (in.token == LPAREN) parArgumentExprss(wrapNew(t))
      else t
    } */

    /** Template          ::=  ConstrApps [TemplateBody] | TemplateBody
     *  ConstrApps        ::=  ConstrApp {`with' ConstrApp}
     *
     *  @return  a pair consisting of the template, and a boolean which indicates
     *           whether the template misses a body (i.e. no {...} part).
     */
    /* def template(constr: DefDef): (Template, Boolean) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) (templateBodyOpt(constr, Nil), false)
      else {
        val parents = tokenSeparated(WITH, constrApp)
        newLineOptWhenFollowedBy(LBRACE)
        val missingBody = in.token != LBRACE
        (templateBodyOpt(constr, parents), missingBody)
      }
    } */

    /** TemplateOpt = [`extends' Template | TemplateBody]
     */
    /* def templateOpt(constr: DefDef): Template =
      if (in.token == EXTENDS) { in.nextToken(); template(constr)._1 }
      else {
        newLineOptWhenFollowedBy(LBRACE)
        if (in.token == LBRACE) template(constr)._1
        else Template(constr, Nil, EmptyValDef, Nil)
      } */

    /** TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     */
    /* def templateBodyOpt(constr: DefDef, parents: List[Tree]) = {
      val (self, stats) =
        if (in.token == LBRACE) templateBody() else (EmptyValDef, Nil)
      Template(constr, parents, self, stats)
    }

    def templateBody(): (ValDef, List[Tree]) = {
      val r = inDefScopeBraces { templateStatSeq() }
      if (in.token == WITH) {
        syntaxError(EarlyDefinitionsNotSupported())
        in.nextToken()
        template(emptyConstructor)
      }
      r
    } */

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
    /* def templateStatSeq(): (ValDef, List[Tree]) = checkNoEscapingPlaceholders {
      var self: ValDef = EmptyValDef
      val stats = new ListBuffer[Tree]
      if (isExprIntro) {
        val first = expr1()
        if (in.token == ARROW) {
          first match {
            case Typed(tree @ This(EmptyTypeIdent), tpt) =>
              self = makeSelfDef(nme.WILDCARD, tpt).withPos(first.pos)
            case _ =>
              val ValDef(name, tpt, _) = convertToParam(first, expected = "self type clause")
              if (name != nme.ERROR)
                self = makeSelfDef(name, tpt).withPos(first.pos)
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
          stats ++= importClause()
        else if (isExprIntro)
          stats += expr1()
        else if (isDefIntro(modifierTokensOrCase))
          stats +++= defOrDcl(in.offset, defAnnotsMods(modifierTokens))
        else if (!isStatSep) {
          exitOnError = mustStartStat
          syntaxErrorOrIncomplete("illegal start of definition")
        }
        acceptStatSepUnlessAtEnd()
      }
      (self, if (stats.isEmpty) List(EmptyTree) else stats.toList)
    } */

    /** RefineStatSeq    ::= RefineStat {semi RefineStat}
     *  RefineStat       ::= Dcl
     *                     |
     *  (in reality we admit Defs and filter them out afterwards)
     */
    /* def refineStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        if (isDclIntro) {
          stats += defOrDcl(in.offset, Modifiers())
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

    def localDef(start: Int, implicitMods: Modifiers = EmptyModifiers): Tree = {
      var mods = defAnnotsMods(localModifierTokens)
      for (imod <- implicitMods.mods) mods = addMod(mods, imod)
      defOrDcl(start, mods)
    } */

    /** BlockStatSeq ::= { BlockStat semi } [ResultExpr]
     *  BlockStat    ::= Import
     *                 | Annotations [implicit] [lazy] Def
     *                 | Annotations LocalModifiers TmplDef
     *                 | Expr1
     *                 |
     */
    /* def blockStatSeq(): List[Tree] = checkNoEscapingPlaceholders {
      val stats = new ListBuffer[Tree]
      var exitOnError = false
      while (!isStatSeqEnd && in.token != CASE && !exitOnError) {
        setLastStatOffset()
        if (in.token == IMPORT)
          stats ++= importClause()
        else if (isExprIntro)
          stats += expr(Location.InBlock)
        else if (isDefIntro(localModifierTokens))
          if (in.token == IMPLICIT) {
            val start = in.offset
            val imods = implicitMods()
            if (isBindingIntro) stats += implicitClosure(start, Location.InBlock, imods)
            else stats +++= localDef(start, imods)
          } else {
            stats +++= localDef(in.offset)
          }
        else if (!isStatSep && (in.token != CASE)) {
          exitOnError = mustStartStat
          val addendum = if (isModifier) " (no modifiers allowed here)" else ""
          syntaxErrorOrIncomplete("illegal start of statement" + addendum)
        }
        acceptStatSepUnlessAtEnd(CASE)
      }
      stats.toList
    } */

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
