package sambaai

import cats.*
import cats.data.*
import cats.syntax.eq.given
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import cats.instances.option.given
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import sambaai.Generators.given
import sambaai.D.{*, given}

object DSpec extends Properties("D"):

  property("f(identity)") = forAll { (d: D[Int]) =>
      D.f[Id, Int, Int](identity)(summon[Monad[Id]])(d) === d
  }

  property("f(Some(_))") = forAll { (d: D[Int]) =>
      D.f[Option, Int, Int](Some(_))(summon[Monad[Option]])(d) === Some(d)
  }

  property("foldLeft(sum)") = forAll { (d: D[Int]) =>
    d.foldLeft(0)(_ + _) === traverseSum(d)
  }

  property("foldRight(sum)") = forAll { (d: D[Int]) =>
    val foldedSum = d.foldRight(Eval.always(0))((sum, current) => Eval.always(sum + current.value)).value

    foldedSum === traverseSum(d)
  }

  property("fold order") = forAll { (d: D[Int]) =>
    val lResult = d.foldLeft(List.empty[Int])(_.+:(_))
    val rResult = d.foldRight(Eval.always(List.empty[Int]))((x, xs) => Eval.always(x :: xs.value)).value

    lResult.reverse === rResult
  }

  property("monoid left identity") = forAll { (d: D[Int]) =>
    (d |+| Monoid[D[Int]].empty) === d
  }

  property("monoid right identity") = forAll { (d: D[Int]) =>
    (Monoid[D[Int]].empty |+| d) === d
  }

  property("monoid associativity") = forAll { (x: D[Int], y: D[Int], z: D[Int]) =>
    ((x |+| y) |+| z) === (x |+| (y |+| z))
  }

  property("monoid associativity refuted") = forAll { (x: D[List[Int]], y: D[List[Int]], z: D[List[Int]]) =>
    ((x |+| y) |+| z) === (x |+| (y |+| z))
  }

  property("functor identity") = forAll { (d: D[Int]) =>
    d.map(identity) === d
  }

  property("functor composition") = forAll { (d: D[Int]) =>
    val f: Function1[Int, Int] = _ + 2
    val g: Function1[Int, Int] = _ / 4

    d.map(g.andThen(f)) === d.map(g).map(f)
  }

  property("applicative identity") = forAll { (d: D[Int]) =>
    Applicative[D].ap(apId)(d) === d
  }

  property("applicative homomorphism") = forAll { (i: Int) =>
    val f: Function1[Int, Int] = _ * 2
    Applicative[D].ap(Applicative[D].pure(f))(Applicative[D].pure(i)) === Applicative[D].pure(f(i))
  }

  def traverseSum(d: D[Int]): Int =
    d.traverse[State[Int, *], Int](i => State.modify[Int](_ + i).as(i)).run(0).value._1

  val apId: D[Int => Int] = Leaf(identity)
