package sambaai

import cats.*
import cats.syntax.apply.given
import cats.syntax.functor.given
import cats.syntax.traverse.given

enum D[T]:
  case Leaf(value: T)
  case HSplit(left: D[T], right: D[T])
  case VSplit(top: D[T], bottom: D[T])

object D:

  // Type inference is not good enough to make this work (even with the correct parameter order)
  // and the type arguments can't be used in the value definition.
  // But interesting to see two new features in one line: Polymorphic function types and Context functions
  // val f: [M[_], A, B] => Monad[M] ?=> (A => M[B]) => D[A] => M[D[B]] = summon[Traverse[D]].traverse(_)(_)

  def f[M[_]: Monad, A, B](g: A => M[B]): D[A] => M[D[B]] = _.traverse(g)

  given dFoldable: Foldable[D] with
    def foldLeft[A, B](da: D[A], b: B)(f: (B, A) => B): B =
      da match
        case Leaf(a) => f(b, a)
        case HSplit(left, right) =>
          val leftResult = foldLeft(left, b)(f)
          foldLeft(right, leftResult)(f)
        case VSplit(top, bottom) =>
          val topResult = foldLeft(top, b)(f)
          foldLeft(bottom, topResult)(f)

    def foldRight[A, B](da: D[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      da match
        case Leaf(a) => f(a, lb)
        case HSplit(left, right) =>
          val rightResult = foldRight(right, lb)(f)
          foldRight(left, rightResult)(f)
        case VSplit(top, bottom) =>
          val bottomResult = foldRight(bottom, lb)(f)
          foldRight(top, bottomResult)(f)

  given dTraverse: Traverse[D] with
    def foldLeft[A, B](da: D[A], b: B)(f: (B, A) => B): B = dFoldable.foldLeft(da, b)(f)
    def foldRight[A, B](da: D[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = dFoldable.foldRight(da, lb)(f)
    def traverse[G[_]: Applicative, A, B](da: D[A])(f: A => G[B]): G[D[B]] =
      da match
        case Leaf(a) => f(a).map(Leaf(_))
        case HSplit(left, right) =>
          Applicative[G].map2(
            traverse(left)(f),
            traverse(right)(f)
          )(HSplit(_, _))
        case VSplit(top, bottom) =>
          Applicative[G].map2(
            traverse(top)(f),
            traverse(bottom)(f)
          )(VSplit(_, _))
