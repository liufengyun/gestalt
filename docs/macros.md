# Portable Macros Based on Platform-Independent Quasiquotes

## Problem

Currently, macro definitions bind statically to one particular implementation of trees, which creates a big portability problem, as IDEs like Intellij have their own trees and typer. How to make macros portable to have better IDE support has been a big headache of Scala compiler developers for a long time.

ScalaMeta is proposed to create a standard set of trees to solve the portability problem by converting all trees to standard trees. However, it makes the implementation of semantic macros difficult and incurs significant engineering efforts in the conversion between trees. Keeping original positions of trees before and after conversion is also a problem.

## Solution

Why bind macro definitions to a specific implementation of trees, instead of just specifying how to compose trees based on a standard set of constructors and extractors? Quasiquotes can also be implemented _platform-independently_ using the standard set of constructors and extractors.

The macro definition only needs to specify how to compose trees using platform-independent quasiquotes, standard constructors and extractors, without commiting to one particular implementation of constructors and extractors. When macros get expanded, it's up to the compiler to pass a particular implementation of constructors and extractors to expand the macros.

Implementation of constructors and deconstructors is simpler than conversion back and forth between ScalaMeta trees and compiler trees. Platform-independent quasiquotes makes it unnecessary to have one implementation of quasiquotes for each Scala compiler.

As there's no conversion between trees, supporting semantic APIs is super easy. Also, positions will not be a problem at all.

## Why it works

How we can be sure that we are able to define a standard set of constructors and extractors and implement quasiquotes based on them? The question is the same as asking:

> Why we are able to define neutral ScalaMeta trees and convert other trees from and to ScalaMeta trees?

To put it another way, if syntactic ScalaMeta trees can succeed, then platform-independent quasiquote can succeed as well.

ScalaMeta still plays an important role in this proposal -- we can rely on the solid implementation of parser and rich meta trees to implement the platform-independent quasiquotes. Also, it serves as a reference to define the standard constructors and extractors.

## Usage

Given the following macro:

```Scala
class main extends StaticAnnotation {
  macro def apply(defn: Any): Any = {
    val q"object $name { ..$stats }" = defn
    val main = q"""
      def stub(args: Any): Any = { ..$stats }
    """
    q"object $name { $main }"
  }
}
```

We can transform it as follows:

```Scala
 class main extends StaticAnnotation {
   macro def apply(defn: Any): Any = null
 }
 
 object main$inline {
   macro def apply(t: Toolbox)(prefix: t.Tree)(T: t.Tree)(defn: t.Tree): t.Tree = ...
 }
```

Similarly, we transform the following def macro:
```Scala
class plus {
  macro def apply(a: Int, b: Int): Int = {
    q"$a + $b"
  }
}
```
to
```Scala
class plus {
  macro def apply(a: Int, b: Int): Int = null
}

object plus$inline {
  def apply(t: Toolbox)(prefix: t.Tree)(a: t.Tree)(b: t.Tree): t.Tree =
    t.Infix(t.Ident("a"), "+", t.Ident("b"))
}
```

## Design

```Scala
trait Toolbox { t =>
  // portable trees
  type Tree <: { def tpe: t.Type }
  type Type
  
  // type operations
  implicit class TypeHelper(val tp1: Type) {
    def =:=(tp2: Type) = t.=:=(tp1, tp2)
    def <:<(tp2: Type) = t.<:<(tp1, tp2)
  }
  def =:=(tp1: t.Type, tp2: t.Type): Boolean = ...
  def <:<(tp1: t.Type, tp2: t.Type): Boolean = ...
  def typeOf(path: String): t.Type = ...

  // standard constructors and extractors
  object Object {
    def apply(name: String, parents: List[t.Tree], self: Option[t.Tree], stats: List[t.Tree]): t.Tree = ...
    def unapply(tree: t.Tree): Option[(String, List[t.Tree], Option[t.Tree], List[t.Tree])] = ...
  }

  object Class {
    def apply(name: String, types: List[t.Tree], paramss: List[List[t.Tree]], parents: List[t.Tree], self: Option[t.Tree], stats: List[t.Tree]): t.Tree = ...
    def unapply(tree: t.Tree): Option[(String, List[t.Tree], List[List[t.Tree]], List[t.Tree], Option[t.Tree], List[t.Tree])] = ...
  }

  object Trait {
    def apply(name: String, types: List[t.Tree], paramss: List[List[t.Tree]], parents: List[t.Tree], self: Option[t.Tree], stats: List[t.Tree]): t.Tree = ...
    def unapply(tree: t.Tree): Option[(String, List[t.Tree], List[List[t.Tree]], List[t.Tree], Option[t.Tree], List[t.Tree])] = ...
  }

  // .... 
}

// platform quasiquote implementation based on standard constructors and extractors
implicit class Quasiquote(val sc: StringContext) {
  def q(args: Any*): Any = Lift(#Infix(t.Ident("a"), "+", t.Ident("b")))             // t is a name fixed by protocol
}
```

We need to implement quasiquotes based on the standard extractors and constructors. This way, we can have a platform-independent implementation of quasiquotes. Different compilers of the language only need to implement the constructors and extractors, which is much simpler than implementing conversions between two sets of trees.

## Technical Details

### For and Yield

The standard toolbox defines `For` and `Yield`, how does it for implementations which have instead `ForDo` and `ForYield`.

```Scala
object For {
  def apply(enums: List[Tree], body: Tree): Tree = {
    if (body.hasAttachment(Yield)) ForYield(enums, body.removeAttachment(Yield))
    else ForDo(enums, body)
  }
  
  def unapply(tree: Tree) = tree match {
    case ForDo(enums, body) =>
      Some((enums, body))
    case ForYield(enums, body) =>
      Some((enums, body.withAttachment(Yield)))
    case _ => None
  }
}

object Yield {
  def apply(expr: Tree) = {
    expr.withAttachment(Yield)
  }
  
  def unapply(tree: Tree) = {
    if (tree.hasAttachment(Yield)) Some(tree.removeAttachment(Yield))
    else None
  }
}
```


### ValDef and PatDef

The standard toolbox only defines `ValDef`, what if a language has both `ValDef` and `PatDef`?

```Scala
object ValDef {
  def apply(lhs: Tree, tpe: Option[Tree], rhs: Tree): Tree = lhs match {
    case Ident(name) => ValDef(name, tpe, rhs)
    case _ => PatDef(lhs, tpe, rhs)
  }
  def unapply(tree: Tree) = tree match {
    case ValDef(name, tpe, rhs) => Some((Ident(name), tpe, rhs))
    case PatDef(pat, tpe, rhs) => Some((pat, tpe, rhs))
    case _ => None
  }
}
```

For the following code, what's the type of `name`?

```Scala
val q"$mods $name = ???" = defn
```

`name` has the type `Tree`.

### Modifiers

Some implementations have modifiers as trees, some don't. We define all modifiers as constructors and extractors.

If the implementation doesn't have modifiers parsed as trees, it can create dummy trees, which will not exist outside of the toolbox:

```Scala
object Private {
  def apply(within: Tree): Tree = DummyPrivateTree(within)    // dummy trees defined in the language to implement Toolbox
  def unapply(tree: Tree) = tree match {
    case DummyPrivateTree(within) => Some(within)
    case _ => None
  }
}
```

Dummy trees will be transformed to the representation of the specific implementation (e.g., flags) in the constructors of `ValDef`, `DefDef` and etc.

If the implementation does have modifiers as trees, then just use them:

```Scala
object Private {
  def apply(within: Tree): Tree = PrivateTree(within)
  def unapply(tree: Tree) = tree match {
    case PrivateTree(within) => Some(within)
    case _ => None
  }
}
```
