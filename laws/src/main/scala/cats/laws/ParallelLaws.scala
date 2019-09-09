package cats
package laws

/**
 * Laws that must be obeyed by any `cats.Parallel`.
 */
trait ParallelLaws[M[_]] extends NonEmptyParallelLaws[M] {
  val P: Parallel[M]


  def isomorphicPure[A](a: A): IsEq[P.F[A]] =
    P.applicative.pure(a) <-> P.parallel(P.monad.pure(a))
}

object ParallelLaws {
  def apply[M[_]](implicit ev: Parallel[M]): ParallelLaws[M] =
    new ParallelLaws[M] { val P: Parallel[M] = ev }
}
