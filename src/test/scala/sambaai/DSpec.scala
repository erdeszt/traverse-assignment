package sambaai

import cats._
import cats.data._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.monoid._
import cats.syntax.traverse._
import cats.instances.option.given
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import sambaai.Generators.given
import sambaai.D._

object DSpec extends Properties("D"):

  property("f(identity)") = forAll { (d: D[Int]) =>
      D.f[Id, Int, Int](identity)(summon[Monad[Id]])(d) == d
  }

  property("f(Some(_))") = forAll { (d: D[Int]) =>
      D.f[Option, Int, Int](Some(_))(summon[Monad[Option]])(d) == Some(d)
  }

  property("foldLeft(sum)") = forAll { (d: D[Int]) =>
    d.foldLeft(0)(_ + _) == traverseSum(d)
  }

  property("foldRight(sum)") = forAll { (d: D[Int]) =>
    val foldedSum = d.foldRight(Eval.always(0))((sum, current) => Eval.always(sum + current.value)).value

    foldedSum == traverseSum(d)
  }

  property("fold order") = forAll { (d: D[Int]) =>
    val lResult = d.foldLeft(List.empty[Int])(_.+:(_))
    val rResult = d.foldRight(Eval.always(List.empty[Int]))((x, xs) => Eval.always(x :: xs.value)).value

    lResult.reverse == rResult
  }

  def traverseSum(d: D[Int]): Int =
    d.traverse[State[Int, *], Int](i => State.modify[Int](_ + i).as(i)).run(0).value._1
