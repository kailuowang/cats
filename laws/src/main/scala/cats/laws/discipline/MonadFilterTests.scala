package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait MonadFilterTests[F[_]] extends MonadTests[F] with FunctorFilterTests[F] {
  def laws: MonadFilterLaws[F]
  def weakLaws: MonadFilterWeakLaws[F]

  def monadFilter[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    EqFInt: Eq[F[Int]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    val weakSet = monadFilterWeak[A, B, C]
    new RuleSet {
      def name: String = "monadFilter"
      def bases: Seq[(String, RuleSet)] = weakSet.bases
      def parents: Seq[RuleSet] = Seq(weakSet)
      def props: Seq[(String, Prop)] = Seq(
        "monadFilter right empty" -> forAll(laws.monadFilterRightEmpty[A, B] _)
      )
    }
  }

  def monadFilterWeak[A: Arbitrary: Eq, B: Arbitrary: Eq, C: Arbitrary: Eq](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFAtoB: Arbitrary[F[A => B]],
    ArbFBtoC: Arbitrary[F[B => C]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqFABC: Eq[F[(A, B, C)]],
    EqFInt: Eq[F[Int]],
    iso: Isomorphisms[F]
  ): RuleSet = {
    new RuleSet {
      def name: String = "monadFilterWeak"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(monad[A, B, C], functorFilter[A, B, C])
      def props: Seq[(String, Prop)] = Seq(
        "monadFilter left empty" -> forAll(weakLaws.monadFilterLeftEmpty[A, B] _),
        "monadFilter consistency" -> forAll(weakLaws.monadFilterConsistency[A, B] _)
      )
    }
  }
}

object MonadFilterTests {
  def apply[F[_]: MonadFilter]: MonadFilterTests[F] =
    new MonadFilterTests[F] {
      def laws: MonadFilterLaws[F] = MonadFilterLaws[F]
      def weakLaws: MonadFilterWeakLaws[F] = MonadFilterWeakLaws[F]
    }
}
