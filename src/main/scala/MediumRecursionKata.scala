// MediumRecursionKata.scala
// Scala 3.x — Exercices récursifs (difficulté moyenne)
// Objectif: travailler récursion structurelle, backtracking, DFS, DP top-down, parsing

import scala.annotation.tailrec

object MediumRecursionKata:

  // ------------------------------------------------------------
  // Helpers
  // ------------------------------------------------------------

  type Graph[A] = Map[A, Set[A]]

  // ------------------------------------------------------------
  // 1) distinctPreserveOrder
  // ------------------------------------------------------------

  def distinctPreserveOrder[A](xs: List[A]): List[A] =
    // TODO: enlever doublons en gardant le premier occurrence + ordre
    def loop(xs: List[A], seen: Set[A] = Set.empty[A]): List[A] =
      xs match
        case Nil => Nil
        case head :: tail =>
          if seen(head) then loop(tail, seen) else head :: loop(tail, seen + head)

    loop(xs)


  // ------------------------------------------------------------
  // 2) pack: regrouper les éléments consécutifs identiques
  // ------------------------------------------------------------

  def pack2[A](xs: List[A]): List[List[A]] =
    // TODO
    // ex: pack(List(1,1,2,3,3,3,2)) == List(List(1,1), List(2), List(3,3,3), List(2))
    def loop(xs: List[A]): List[List[A]] =
      xs match
        case Nil => Nil
        case head :: tail =>
          loop(tail) match
            case Nil => List(List(head))
            case (grp @ (headGrp ::_)) :: rest =>
              if headGrp == head then (head :: grp) :: rest else List(head) :: grp :: rest

    loop(xs)

  def pack[A](xs: List[A]): List[List[A]] =
    // TODO
    // ex: pack(List(1,1,2,3,3,3,2)) == List(List(1,1), List(2), List(3,3,3), List(2))
    def takeSame(a: A, in: List[A]): (List[A], List[A]) =
      in match
        case head :: tail if a == head =>
          val (previousWithA, previousRest) = takeSame(a, tail)
          (a :: previousWithA, previousRest)
        case _ => (List(a), in)

    xs match
      case Nil => Nil
      case head :: tail =>
        val (same, rest) = takeSame(head, tail)
        same :: pack(rest)


  // ------------------------------------------------------------
  // 3) lengthR: longueur d'une liste (récursion structurelle)
  // ------------------------------------------------------------

  def lengthR[A](xs: List[A]): Int =
    // TODO: sans .length
    xs match
      case Nil => 0
      case _ :: tail => 1 + lengthR(tail)


  // ------------------------------------------------------------
  // 4) sumDecompositions: toutes les décompositions en somme (sans permutations)
  // ------------------------------------------------------------

  def sumDecompositions(n: Int): List[List[Int]] =
    // TODO:
    // - renvoyer toutes les listes d'entiers positifs (>=1) dont la somme vaut n
    // - sans permutations: on impose un ordre non-croissant, ou on force un "max" autorisé
    //
    // ex n=4:
    //   List(4)
    //   List(3,1)
    //   List(2,2)
    //   List(2,1,1)
    //   List(1,1,1,1)
    val cache = scala.collection.mutable.Map.empty[(Int, Int), List[List[Int]]]
    def combos(remaining: Int, maxAllowed: Int): List[List[Int]] =
      cache.getOrElseUpdate((remaining, maxAllowed), {
        if remaining == 0 then List(Nil)
        else
          (1 to math.min(remaining, maxAllowed)).flatMap(current => combos(remaining - current, current).map(current :: _)).toList
      })

    combos(n, n)


  def sumDecompositions2(n: Int): List[List[Int]] =
    // TODO:
    // - renvoyer toutes les listes d'entiers positifs (>=1) dont la somme vaut n
    // - sans permutations: on impose un ordre non-croissant, ou on force un "max" autorisé
    //
    // ex n=4:
    //   List(4)
    //   List(3,1)
    //   List(2,2)
    //   List(2,1,1)
    //   List(1,1,1,1)
    val cache = scala.collection.mutable.Map.empty[Int, List[List[Int]]]

    def loop(remaining: Int): List[List[Int]] =
      cache.getOrElseUpdate(remaining, {
        if remaining == 0 then List(Nil)
        else
          (1 to remaining).flatMap(current => loop(remaining - current).map(current :: _)).toList
      })

    loop(n)


  // ------------------------------------------------------------
  // 5) minSumDecomposition: une décomposition "courte" (greedy ne suffit pas toujours)
  // ------------------------------------------------------------

  def minSumDecomposition(n: Int): List[Int] =
    // TODO:
    // - renvoyer UNE décomposition de n en entiers positifs
    // - objectif: minimiser la longueur de la liste (nb de termes)
    // Indice: DP top-down ou DFS + best-so-far
    ???


  // ------------------------------------------------------------
  // 6) existsPathPassingThrough: existe-t-il un chemin start->end passant par tous mustPass ?
  // ------------------------------------------------------------

  def existsPathPassingThrough[A](
                                   g: Graph[A],
                                   start: A,
                                   end: A,
                                   mustPass: Set[A]
                                 ): Boolean =
    // TODO:
    // - graphe quelconque, éviter cycles
    // - état recommandé: (current, remainingMustPass)
    def loop(current: A, remaining: Set[A], explored: Set[A]): Boolean =
      if current == end then
        remaining.isEmpty
      else
        g(current).diff(explored).exists(c => loop(c, remaining - c, explored + c))

    loop(start, mustPass - start, Set(start))

  // ------------------------------------------------------------
  // 7) shortestPathNoCycles: plus court chemin (en nb d'arêtes) sans revisiter de noeud
  // ------------------------------------------------------------

  def shortestPathNoCycles[A](g: Graph[A], start: A, end: A): Option[Int] =
    import scala.math.Ordering.Implicits.given
    // TODO:
    // - renvoyer Some(distance) si un chemin simple existe, sinon None
    // - DFS possible avec pruning (best-so-far)
    def loop(from: A, visited: Set[A], depth: Int, best: Option[Int]): Option[Int] =
      if best.exists(_ <= depth) then best
      else if from == end then Some(depth)
      else
        g.getOrElse(from, Set.empty[A]).diff(visited).foldLeft(best):
          case (newBest, next) =>
            loop(next, visited + from, depth + 1, newBest) max newBest

    loop(start, Set(start), 0, None)


  def shortestPathNoCycles2[A](g: Graph[A], start: A, end: A): Option[Int] =
    // TODO:
    // - renvoyer Some(distance) si un chemin simple existe, sinon None
    // - DFS possible avec pruning (best-so-far)
    @tailrec
    def loop(frontier: Set[A], visited: Set[A], depth: Int): Option[Int] =
      if frontier.isEmpty then None
      else if frontier.contains(end) then Some(depth)
      else
        val nextFrontier = frontier.iterator
                              .flatMap(n => g.getOrElse(n, Set.empty).iterator)
                                  .filterNot(visited)
                                  .toSet
        loop(nextFrontier, visited ++ frontier, depth + 1)

    loop(Set(start), Set(start), 0)


  // ------------------------------------------------------------
  // 8) countWays: escaliers - nb de façons d'atteindre n avec des pas autorisés
  // ------------------------------------------------------------

  def countWays(n: Int, steps: List[Int]): Long =
    require(n >= 0)
    require(steps.nonEmpty)
    require(steps.forall(_ > 0))
    // TODO:
    // - nb de séquences de pas (ordre important) qui somment à n
    // - ex: n=4, steps=List(1,2) => 5
    // Indice: DP memo(n)
    val cache = scala.collection.mutable.Map.empty[Int, Long]
    def loop(remainingSteps: Int): Long =
      cache.getOrElseUpdate(remainingSteps, {
        remainingSteps match
          case 0 => 1L
          case notZero =>
            steps.map(notZero - _).filter(_ >= 0).map(loop).sum
      })

    loop(n)

  // ------------------------------------------------------------
  // 9) wordBreak: découpe de chaîne en mots d'un dictionnaire
  // ------------------------------------------------------------

  def wordBreak(s: String, dict: Set[String]): Boolean =
    // TODO:
    // - vrai si s peut être segmentée en mots de dict
    // - DP top-down sur index i
    @tailrec
    def loop(index: Int, word: String, matches: Int = 0): Option[Int] =
      if word.isEmpty then Some(matches)
      else
        if word.headOption.contains(s.charAt(index)) then loop(index + 1, word.tail, matches + 1)
        else None
    
    if s.isEmpty then true
    else
      dict.exists:
        word =>
          loop(0, word).exists(newIndex => wordBreak(s.drop(newIndex), dict))




    /*@tailrec
    def loop(index: Int, subDict: Set[String]): Boolean =
      if index == s.length then true
      else
        val nextSubDict = subDict.filter(_.head == s.charAt(index)).map(_.tail)
        if nextSubDict.isEmpty then false
        else
          if nextSubDict.exists(_.isEmpty) then loop(index + 1, dict)
          else loop(index + 1, nextSubDict)

    dict.map(loop(0, _))*/



  // ------------------------------------------------------------
  // 10) Mini parsing JSON: nombres et tableaux
  // ------------------------------------------------------------

  sealed trait J
  case class JNum(n: Int) extends J
  case class JArr(values: List[J]) extends J

  def parseJ(s: String): Either[String, J] =
    // TODO:
    // grammaire (espaces optionnels):
    // value := number | array
    // number := [0-9]+
    // array := '[' (value (',' value)*)? ']'
    //
    // Conseillé: parseValue(i): Either[String, (J, nextIndex)]
    // puis vérifier consommation complète (hors espaces)
    ???


  // ------------------------------------------------------------
  // TESTS
  // ------------------------------------------------------------

  private def assertEq[A](got: A, expected: A): Unit =
    assert(got == expected, s"Expected: $expected, got: $got")

  private def assertSetEq[A](got: List[List[A]], expected: Set[List[A]]): Unit =
    assert(got.toSet == expected, s"Expected(set): $expected, got(set): ${got.toSet}")

  def main(args: Array[String]): Unit =
    // 1) distinctPreserveOrder
    assertEq(distinctPreserveOrder(List(1, 2, 1, 3, 2, 4)), List(1, 2, 3, 4))
    assertEq(distinctPreserveOrder(List("a", "a", "b", "a")), List("a", "b"))
    assertEq(distinctPreserveOrder(Nil: List[Int]), Nil)

    // 2) pack
    assertEq(pack(List(1, 1, 2, 3, 3, 3, 2)), List(List(1, 1), List(2), List(3, 3, 3), List(2)))
    assertEq(pack(List("a", "a", "a")), List(List("a", "a", "a")))
    assertEq(pack(Nil: List[Int]), Nil)

    assertEq(pack2(List(1, 1, 2, 3, 3, 3, 2)), List(List(1, 1), List(2), List(3, 3, 3), List(2)))
    assertEq(pack2(List("a", "a", "a")), List(List("a", "a", "a")))
    assertEq(pack2(Nil: List[Int]), Nil)

    // 3) lengthR
    assertEq(lengthR(Nil: List[Int]), 0)
    assertEq(lengthR(List(10, 20, 30, 40)), 4)

    // 4) sumDecompositions
    val d4 = sumDecompositions(4).map(_.sorted(Ordering.Int.reverse)) // normalise en non-croissant
    assertSetEq(
      d4,
      Set(
        List(4),
        List(3, 1),
        List(2, 2),
        List(2, 1, 1),
        List(1, 1, 1, 1)
      )
    )

    // 5) minSumDecomposition
    // On vérifie "somme" et "longueur minimale"
    /*val m7 = minSumDecomposition(7)
    assertEq(m7.sum, 7)
    assertEq(m7.length, 1) // car List(7) est autorisée (entiers positifs)
    val m0 = minSumDecomposition(0)
    assertEq(m0.sum, 0)
    assertEq(m0, Nil)*/

    // 6) existsPathPassingThrough
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
    assertEq(existsPathPassingThrough(g, "svr", "out", Set("fft", "dac")), true)
    assertEq(existsPathPassingThrough(g, "svr", "out", Set("dac", "tty")), true)
    assertEq(existsPathPassingThrough(g, "svr", "out", Set("dac", "tty", "fft")), false)

    // 7) shortestPathNoCycles
    // Sur ce graphe, toutes les routes passent par ccc puis fff puis (ggg|hhh) puis out.
    // La plus courte: svr->bbb->tty->ccc->ddd->hub->fff->ggg->out (ou via hhh) : 8 arêtes
    assertEq(shortestPathNoCycles(g, "svr", "out"), Some(8))
    assertEq(shortestPathNoCycles(g, "out", "svr"), None)

    assertEq(shortestPathNoCycles2(g, "svr", "out"), Some(8))
    assertEq(shortestPathNoCycles2(g, "out", "svr"), None)

    // 8) countWays
    assertEq(countWays(0, List(1, 2)), 1L) // une façon: ne rien faire
    assertEq(countWays(4, List(1, 2)), 5L) // classic
    assertEq(countWays(7, List(2, 4)), 0L)

    // 9) wordBreak
    assertEq(wordBreak("leetcode", Set("leet", "code")), true)
    assertEq(wordBreak("applepenapple", Set("apple", "pen")), true)
    assertEq(wordBreak("catsandog", Set("cats", "dog", "sand", "and", "cat")), false)

    // 10) parseJ
    /*assertEq(parseJ("123"), Right(JNum(123)))
    assertEq(parseJ("[1,2,3]"), Right(JArr(List(JNum(1), JNum(2), JNum(3)))))
    assertEq(parseJ("[1,[2,3]]"), Right(JArr(List(JNum(1), JArr(List(JNum(2), JNum(3)))))))
    assert(parseJ("]").isLeft)
    assert(parseJ("[1,]").isLeft)

    println("✅ Tous les tests passent (si tu as complété les TODO).")*/

end MediumRecursionKata
