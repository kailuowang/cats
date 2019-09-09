package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait ParallelTests[M[_]] extends NonEmptyParallelTests[M] {
  val laws: ParallelLaws[M]

  def parallel[A, B](implicit ArbA: Arbitrary[A],
                     ArbM: Arbitrary[M[A]],
                     ArbMb: Arbitrary[M[B]],
                     Arbf: Arbitrary[A => B],
                     EqMa: Eq[M[A]],
                     EqMb: Eq[M[B]],
                     ArbF: Arbitrary[F[A]],
                     EqFa: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      "parallel",
      Some(nonEmptyParallel[A, B]),
      "isomorphic pure" -> forAll((a: A) => laws.isomorphicPure(a))
    )
}

object ParallelTests {
  def apply[M[_]](implicit ev: Parallel[M]): ParallelTests[M] =
    new ParallelTests[M] { val laws: ParallelLaws[M] = ParallelLaws[M] }
}
