package cats.tests

import cats.instances.either.catsStdEqForEither
import cats.instances.int._
import cats.instances.string._
import cats.laws.discipline.ParallelTests
import cats.laws.discipline.arbitrary._

class ParallelMonadErrorInstanceSuite  extends CatsSuite {
  import cats.instances.either.catsStdInstancesForEither
  implicit val m = cats.Parallel.parallelForMonadError[Either[String, *], String]

  checkAll("Parallel[Either[String, *]", ParallelTests[Either[String, *]].parallel[Int, String])
}
