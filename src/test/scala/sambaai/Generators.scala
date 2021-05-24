package sambaai

import org.scalacheck._
import org.scalacheck.Arbitrary._

object Generators:

  val MAX_SIZE = 10

  def genLeaf[T](tGen: Gen[T]): Gen[D[T]] = tGen.map(D.Leaf(_))

  def genHSplit[T](tGen: Gen[T], size: Int): Gen[D[T]] =
    if (size > 0) then
      for
        left <- genD(tGen, size - 1)
        right <- genD(tGen, size - 1)
      yield D.HSplit(left, right)
    else
      genLeaf(tGen)

  def genVSplit[T](tGen: Gen[T], size: Int): Gen[D[T]] =
    if (size > 0) then
      for
        top <- genD(tGen, size - 1)
        bottom <- genD(tGen, size - 1)
      yield D.VSplit(top, bottom)
    else
      genLeaf(tGen)

  def genD[T](tGen: Gen[T], size: Int): Gen[D[T]] =
    Gen.oneOf(genLeaf(tGen), genHSplit(tGen, size), genVSplit(tGen, size))

  given dArbitrary[T](using tArb: Arbitrary[T]): Arbitrary[D[T]] = Arbitrary(genD(tArb.arbitrary, MAX_SIZE))


