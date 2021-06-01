package sambaai

import cats.*
import cats.syntax.apply.given
import cats.syntax.eq.given
import cats.syntax.functor.given
import cats.syntax.semigroup.given
import cats.syntax.traverse.given

enum D[T]:
  case Leaf(value: T)
  case HSplit(left: D[T], right: D[T])
  case VSplit(top: D[T], bottom: D[T])


trait LowPriorityInstances0:
  import D.*
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

  given dFunctor: Functor[D] with
    def map[A, B](fa: D[A])(f: A => B): D[B] =
      fa match
        case Leaf(value) => Leaf(f(value))
        case HSplit(left, right) => HSplit(map(left)(f), map(right)(f))
        case VSplit(top, bottom) => VSplit(map(top)(f), map(bottom)(f))

trait LowPriorityInstances1 extends LowPriorityInstances0:
  import D.*
  given dTraverse: Traverse[D] with
    def foldLeft[A, B](da: D[A], b: B)(f: (B, A) => B): B = dFoldable.foldLeft(da, b)(f)
    def foldRight[A, B](da: D[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = dFoldable.foldRight(da, lb)(f)
    override def map[A, B](fa: D[A])(f: A => B): D[B] = dFunctor.map(fa)(f)
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

  given dSemigroup[T: Semigroup]: Semigroup[D[T]] with
    def combine(lhs: D[T], rhs: D[T]): D[T] =
      (lhs, rhs) match
        case (Leaf(l), Leaf(r)) => Leaf(l |+| r)
        // Leaf + H or V adds the leaf to the right side
        case (Leaf(l), HSplit(rl, rr)) => HSplit(rl, combine(Leaf(l), rr))
        case (HSplit(ll, lr), Leaf(r)) => HSplit(ll, combine(lr, Leaf(r)))
        case (Leaf(l), VSplit(rl, rr)) => VSplit(rl, combine(Leaf(l), rr))
        case (VSplit(ll, lr), Leaf(r)) => VSplit(ll, combine(lr, Leaf(r)))
        // H + H and V + V combines the two sides separately
        case (HSplit(ll, lr), HSplit(rl, rr)) => HSplit(combine(ll, rl), combine(lr, rr))
        case (VSplit(ll, lr), VSplit(rl, rr)) => VSplit(combine(ll, rl), combine(lr, rr))
        // In case of different splits the left hand side takes priority
        case (HSplit(ll, lr), VSplit(rl, rr)) => HSplit(combine(ll, rl), combine(lr, rr))
        case (VSplit(ll, lr), HSplit(rl, rr)) => VSplit(combine(ll, rl), combine(lr, rr))

  given dApply: Apply[D] with
    def map[A, B](fa: D[A])(f: A => B): D[B] = dFunctor.map(fa)(f)
    def ap[A, B](ff: D[A => B])(fa: D[A]): D[B] =
      ff match
        case Leaf(f) => map(fa)(f)
        case HSplit(fl, fr) => HSplit(ap(fl)(fa) , ap(fr)(fa))
        case VSplit(fl, fr) => VSplit(ap(fl)(fa) , ap(fr)(fa))

object D extends LowPriorityInstances1:

  // Type inference is not good enough to make this work (even with the correct parameter order)
  // and the type arguments can't be used in the value definition.
  // But interesting to see two new features in one line: Polymorphic function types and Context functions
  // val f: [M[_], A, B] => Monad[M] ?=> (A => M[B]) => D[A] => M[D[B]] = summon[Traverse[D]].traverse(_)(_)

  def f[M[_]: Monad, A, B](g: A => M[B]): D[A] => M[D[B]] = _.traverse(g)

  given dMonoid[T](using tMonoid: Monoid[T]): Monoid[D[T]] with
    def empty: D[T] = Leaf(tMonoid.empty)
    def combine(lhs: D[T], rhs: D[T]): D[T] = dSemigroup.combine(lhs, rhs)

  given dApplicative: Applicative[D] with
    override def map[A, B](fa: D[A])(f: A => B): D[B] = dFunctor.map(fa)(f)
    def ap[A, B](ff: D[A => B])(fa: D[A]): D[B] = dApply.ap(ff)(fa)
    def pure[A](value: A): D[A] = Leaf(value)

  given eq[T: Eq]: Eq[D[T]] with
    def eqv(lhs: D[T], rhs: D[T]): Boolean =
      (lhs, rhs) match
        case (Leaf(l), Leaf(r)) => l === r
        case (HSplit(ll, lr), HSplit(rl, rr)) => ll === rl && lr === rr
        case (VSplit(ll, lr), VSplit(rl, rr)) => ll === rl && lr === rr
        case _ => false
