// RecursionKata.scala
// Scala 3.x — kata de récursion progressive
// Objectif: écrire TOUT en récursif (pas de .map/.filter/.fold/.reverse/.sum/.max etc.)
// Tu as le droit aux constructions de base: match, if, ::, Nil, Option, Either, Map/Set, etc.

import scala.annotation.tailrec


final class CountingCache[K, V]:
  private val m = scala.collection.mutable.HashMap.empty[K, V]
  private var _hits: Long = 0L
  private var _misses: Long = 0L

  def hits: Long = _hits
  def misses: Long = _misses
  def size: Int = m.size

  inline def getOrElseUpdate(key: K, compute: => V): V =
    m.get(key) match
      case Some(v) =>
        _hits += 1
        v
      case None =>
        _misses += 1
        val v = compute
        m.put(key, v)
        v

object RecursionKata:

  // ------------------------------------------------------------
  // 1) LISTES — récursion structurelle
  // ------------------------------------------------------------

  def sum(xs: List[Int]): Int =
    xs match
      case Nil => 0
      case head :: tail => head + sum(tail)

  def product(xs: List[Int]): Long =
    xs match
      case Nil => 1L
      case head :: tail => head.toLong * product(tail)

  def maxOption(xs: List[Int]): Option[Int] =
    xs match
      case Nil => None
      case head :: tail => maxOption(tail) match
        case Some(value) => Some(math.max(head, value))
        case None => Some(head)

  def countWhere(xs: List[Int])(p: Int => Boolean): Int =
    xs match
      case Nil => 0
      case head :: tail =>(if p.apply(head) then 1 else 0) + countWhere(tail)(p)

  def sumUntil(xs: List[Int])(stop: Int): Int =
    xs match
      case Nil => 0
      case value :: tail if value == stop => 0
      case head :: tail => head + sumUntil(tail)(stop)


  // ------------------------------------------------------------
  // 2) ACCUMULATEURS — tail recursion
  // ------------------------------------------------------------

  def reverse[A](xs: List[A]): List[A] =
    @tailrec
    def loop(rest: List[A], alreadyBuilt: List[A]): List[A] =
      rest match
        case Nil => alreadyBuilt
        case head :: tail => loop(tail, rest.head +: alreadyBuilt)

    loop(xs, Nil)

  def mapR[A, B](xs: List[A])(f: A => B): List[B] =
    xs match
      case Nil => Nil
      case head :: tail => f(head) +: mapR(tail)(f)

  def filterR[A](xs: List[A])(p: A => Boolean): List[A] =
    xs match
      case Nil => Nil
      case head :: tail => p(head) match
        case true => head +: filterR(tail)(p)
        case false => filterR(tail)(p)

  def flatMapR[A, B](xs: List[A])(f: A => List[B]): List[B] =
    @tailrec
    def loop(toAdd: List[B], currentList: List[B]): List[B] =
      toAdd match
        case Nil => currentList
        case head :: tail => loop(tail, head +: currentList)


    xs match
      case Nil => Nil
      case head :: tail => loop(f(head), flatMapR(tail)(f))

  def flatMapR2[A, B](xs: List[A])(f: A => List[B]): List[B] =
    @tailrec
    def loop(toAdd: List[A], currentList: List[B]): List[B] =
      toAdd match
        case Nil => currentList.reverse
        case head :: tail => loop(tail, f(head).reverse ::: currentList)

    loop(xs, Nil)

  // ------------------------------------------------------------
  // 3) ARBRES — récursion naturelle
  // ------------------------------------------------------------

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int =
    t match
      case Leaf(_) => 1
      case Node(left, right) => size(left) + size(right)

  def depth[A](t: Tree[A]): Int =
    t match
      case Leaf(_) => 1
      case Node(left, right) => math.max(1 + depth(left), 1 + depth(right))

  def exists[A](t: Tree[A])(p: A => Boolean): Boolean =
    t match
      case Leaf(leaf) => p(leaf)
      case Node(left, right) => exists(left)(p) || exists(right)(p)

  def toListInOrder[A](t: Tree[A]): List[A] =
    t match
      case Leaf(leaf) => leaf :: Nil
      case Node(left, right) => toListInOrder(left) ++: toListInOrder(right)


  // ------------------------------------------------------------
  // 4) BACKTRACKING — explorer des possibilités
  // ------------------------------------------------------------

  def subsets[A](xs: List[A]): List[List[A]] =
    xs match
      case Nil => List(Nil)
      case head :: tail =>
        val withoutHead = subsets(tail)
        withoutHead ::: withoutHead.map(head +: _)

  def subsetsOfSize[A](xs: List[A], k: Int): List[List[A]] =
    if k == 0 then
      List(Nil)
    else
      xs match
        case Nil => Nil
        case head :: tail =>
          val withoutHead = subsetsOfSize(tail, k - 1)
          subsetsOfSize(tail, k) ::: withoutHead.map(head +: _)

  def permutations[A](xs: List[A]): List[List[A]] =
    def insertEveryWhere(toInsert: A, inList: List[A]): List[List[A]] =
      inList match
        case Nil => List(List(toInsert))
        case head :: tail =>
          (toInsert :: inList) :: insertEveryWhere(toInsert, tail).map(innerList => head +: innerList )

    xs match
      case Nil => List(Nil)
      case head :: tail => permutations(tail).flatMap(insertEveryWhere(head, _))


  // ------------------------------------------------------------
  // 5) GRAPHES — compter des chemins
  // ------------------------------------------------------------

  type Graph[A] = Map[A, Set[A]]

  def countPathsDAG[A](g: Graph[A], start: A, end: A): Long =
    val cache = scala.collection.mutable.Map.empty[A, Long]

    def loop(current: A)(using g: Graph[A]): Long =
      cache.getOrElseUpdate(current, {
        if (current == end)
          1L
        else
          g(current).iterator.map(loop).sum
      })

    loop(start)(using g)

  def countPathsNoCycles[A](g: Graph[A], start: A, end: A): Long =
    // TODO: graphe quelconque, évite les cycles par visited (DFS)

    def loop(current: A, visited: Set[A])(using g: Graph[A]): Long =
      current match
        case c if visited.contains(c) => 0L
        case c if c == end => 1L
        case c  => g(c).iterator.map(loop(_, visited + c)).sum

    loop(start, Set.empty[A])(using g)

  def countPathsPassingThroughDAG[A](
                                      g: Graph[A],
                                      start: A,
                                      end: A,
                                      mustPass: Set[A]
                                    ): Long =
    // TODO: version DAG: compter les chemins start->end qui passent par tous les noeuds de mustPass
    // Indice: transforme l'état en (node, mask) ou (node, remaining)
    def loop(current: A, remaining: Set[A])(using g: Graph[A]): Long =
      val nextRemaining = remaining - current

      current match
        case c if c == end =>
          if nextRemaining.isEmpty then 1L else 0L
        case c  => g(c).iterator.map(loop(_, nextRemaining)).sum

    loop(start, mustPass)(using g)


  // ------------------------------------------------------------
  // 6) DP TOP-DOWN — mémo
  // ------------------------------------------------------------

  def fibMemo(n: Int): Long =
    // TODO: fib(0)=0 fib(1)=1, mémo obligatoire
    val cache = scala.collection.mutable.Map.empty[Int, Long]
    def loop(depth: Int): Long =
      depth match
        case 0 => 0L
        case 1 => 1L
        case other => cache.getOrElseUpdate(other, {
          loop(depth - 1) + loop(depth - 2)
        })

    loop(n)

  def minCoins(amount: Int, coins: List[Int]): Option[Int] =
    require(amount >= 0)
    require(!coins.exists(_ <= 0))

    // TODO: nombre minimum de pièces pour faire amount (>=0), pièces illimitées
    val cache = new CountingCache[Int, Option[Int]]

    def loop(remaining: Int): Option[Int] =
      cache.getOrElseUpdate(remaining, {
      remaining match
        case 0 => Some(0)
        case value => coins.map(remaining - _).filter(_ >= 0).flatMap(loop).map(_ + 1).minOption
      })

    val res = loop(amount)
    println(s"cache hits=${cache.hits}, misses=${cache.misses}, size=${cache.size}")
    res

  // ------------------------------------------------------------
  // 7) RÉCURSION SUR STRING — parenthèses équilibrées + mini parsing
  // ------------------------------------------------------------

  def isBalancedParens(s: String): Boolean =
    // TODO: uniquement '(' et ')', ignorer les autres chars
    // Indice: récursion sur index + compteur de profondeur
    def loop(parOnly: String): Boolean =
      println(s"loop: $parOnly")
      @tailrec
      def findCorresponding(subString: String, depth: Int = 1, index: Int = 0): Option[Int] =
        if index >= subString.length then None
        else
          (subString.charAt(index), depth) match
            case (')', 1) => Some(index)
            case (')', currentDepth) => findCorresponding(subString, currentDepth - 1, index + 1)
            case (_, currentDepth) => findCorresponding(subString, currentDepth + 1, index + 1)

      parOnly match
        case "" => true
        case value if value.length % 2 == 1 => false
        case s"($tail" =>
          val indexO = findCorresponding(tail)
          indexO match
            case Some(index) => loop(tail.take(index)) && loop(tail.drop(index))
            case None => false
        case _ => false

    loop(s.filter(char => char == '(' || char == ')'))

  // Mini-evaluateur d'expression: + et * et parenthèses
  // Grammaire (sans espaces) :
  //   expr   := term (('+' term)*)?
  //   term   := factor (('*' factor)*)?
  //   factor := number | '(' expr ')'
  // number := [0-9]+
  //
  // On retourne Either(error, value)
  def evalExpr(s: String): Either[String, Int] =
    // TODO: parse récursif avec index; retourne erreur lisible si parse impossible ou restant non consommé
    ???

  // ------------------------------------------------------------
  // TESTS
  // ------------------------------------------------------------

  def main(args: Array[String]): Unit =
    // 1) listes
    assert(sum(Nil) == 0)
    assert(sum(List(1, 2, 3)) == 6)

    assert(product(Nil) == 1L)
    assert(product(List(2, 3, 4)) == 24L)

    assert(maxOption(Nil).isEmpty)
    assert(maxOption(List(5, 1, 9, 2)).contains(9))

    assert(countWhere(List(1, 2, 3, 4, 5))(_ % 2 == 0) == 2)
    assert(sumUntil(List(1, 2, 99, 3, 4))(99) == 3)
    assert(sumUntil(List(1, 2, 3))(99) == 6)

    // 2) accumulateurs
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
    assert(mapR(List(1, 2, 3))(_ * 10) == List(10, 20, 30))
    assert(filterR(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))
    assert(flatMapR(List(1, 2, 3))(x => List(x, x)) == List(1, 1, 2, 2, 3, 3))
    assert(flatMapR2(List(1, 2, 3))(x => List(x, x)) == List(1, 1, 2, 2, 3, 3))

    // 3) arbres
    val t: Tree[Int] =
      Node(
        Node(Leaf(1), Leaf(2)),
        Node(Leaf(3), Node(Leaf(4), Leaf(5)))
      )
    assert(size(t) == 5)
    assert(depth(t) == 4)
    assert(exists(t)(_ == 4))
    assert(!exists(t)(_ == 999))
    assert(toListInOrder(t) == List(1, 2, 3, 4, 5))

    // 4) backtracking
    val subs = subsets(List(1, 2))
    assert(subs.map(_.sorted).toSet == Set(List(), List(1), List(2), List(1, 2)))

    val subs2 = subsetsOfSize(List(1, 2, 3), 2).map(_.sorted).toSet
    assert(subs2 == Set(List(1, 2), List(1, 3), List(2, 3)))

    val perms = permutations(List(1, 2, 3)).map(_.toList).toSet
    println(perms)
    assert(perms.size == 6)
    assert(perms.contains(List(1, 2, 3)))
    assert(perms.contains(List(3, 2, 1)))

    // 5) graphes
    val g: Graph[String] = Map(
      "svr" -> Set("aaa", "bbb"),
      "aaa" -> Set("fft"),
      "fft" -> Set("ccc"),
      "bbb" -> Set("tty"),
      "tty" -> Set("ccc"),
      "ccc" -> Set("ddd", "eee"),
      "ddd" -> Set("hub"),
      "hub" -> Set("fff"),
      "eee" -> Set("dac"),
      "dac" -> Set("fff"),
      "fff" -> Set("ggg", "hhh"),
      "ggg" -> Set("out"),
      "hhh" -> Set("out"),
      "out" -> Set.empty
    )

    assert(countPathsDAG(g, "svr", "out") == 8L) // à vérifier: chemins = 4
    assert(countPathsNoCycles(g, "svr", "out") == 8L)
    assert(countPathsPassingThroughDAG(g, "svr", "out", Set("fft", "dac")) == 2L)

    // 6) dp
    assert(fibMemo(0) == 0)
    assert(fibMemo(1) == 1)
    assert(fibMemo(10) == 55)

    assert(minCoins(0, List(1, 3, 4)).contains(0))
    assert(minCoins(6, List(1, 3, 4)).contains(2))  // 3+3
    assert(minCoins(7, List(2, 4)).isEmpty)
    assert(minCoins(6760, List(10, 20, 50, 100, 200)).contains(36))

    // 7) strings
    assert(isBalancedParens("(()())"))
    assert(!isBalancedParens("(()"))
    assert(!isBalancedParens("())("))
    assert(isBalancedParens("a(b)c"))

    /*assert(evalExpr("1+2*3") == Right(7))
    assert(evalExpr("(1+2)*3") == Right(9))
    assert(evalExpr("10*(2+3)") == Right(50))
    assert(evalExpr(")") .isLeft)

    println("✅ Tous les tests passent (si tu as complété les TODO).")*/

end RecursionKata
