package cats
package laws

/**
 * Laws that must be obeyed by any `cats.NonEmptyParallel`.
 */
trait NonEmptyParallelLaws[M[_]] {
  val P: NonEmptyParallel[M]

  def parallelRoundTrip[A](ma: M[A]): IsEq[M[A]] =
    P.sequential(P.parallel(ma)) <-> ma

  def sequentialRoundTrip[A](fa: P.F[A]): IsEq[P.F[A]] =
    P.parallel(P.sequential(fa)) <-> fa

  def isomorphicFunctor[A, B](fa: P.F[A], f: A => B): IsEq[M[B]] =
    P.flatMap.map(P.sequential(fa))(f) <-> P.sequential(P.apply.map(fa)(f))
}

object NonEmptyParallelLaws {
  def apply[M[_]](implicit ev: NonEmptyParallel[M]): NonEmptyParallelLaws[M] =
    new NonEmptyParallelLaws[M] { val P: NonEmptyParallel[M] = ev }
}
