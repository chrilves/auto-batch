package autobatch

import cats.Monad
import cats.data.Writer
import cats.instances.long._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.collection.immutable.TreeSet

class BatchTSpec extends Specification {
  // Size of lists to test to ensure stack safety
  val listMaxSize = 100000L
  val notFoundValue: Long = 1000000000L

  type Request = Long
  type Response = Long

  type F[A] = Writer[Long, A]

  lazy val batchAPI: BatchAPI[F, Request, Response] =
    new BatchAPI[F, Request, Response] {
      lazy val baseMonad: Monad[F] = implicitly[Monad[F]]
      lazy val requestOrdering: Ordering[Request] = Ordering[Long]

      def sendBatch(requests: List[Request]): F[List[Response]] =
        for {
          _ <- Writer.tell(1L) // We count the number of calls!
        } yield requests.filter(_ >= 0)

      def dispatch(requests: List[Request],
                   responses: List[Response]): Map[Request, Response] =
        responses.map(i => (i, i)).toMap

      def notFound(request: Request): F[Response] =
        for {
          _ <- Writer.tell(notFoundValue)
        } yield -request
    }

  import batchAPI.implicits._

  def timedRun[A](name: String, v: batchAPI.Batch[A]): (Long, A) = {
    val t1 = System.currentTimeMillis()
    val x = v.run(batchAPI).run
    println(
      s"${Console.YELLOW}[BatchTSpec.scala] $name: ${System.currentTimeMillis - t1} ms${Console.RESET}")
    x
  }

  def log[A](name: String)(v: A): A = {
    println(s"${Console.GREEN}$name = $v${Console.RESET}")
    v
  }

  "sendRequest" should {
    "Collect all the requests" in {
      val requests = List.range(1L, listMaxSize + 1)
      import lib.instances.list._

      requests.traverse(batchAPI.sendRequest).requests === (TreeSet
        .empty[Request] ++ requests)
    }

    "Make no calls if not required" in {
      timedRun("Make no calls if not required", batchAPI.pure("Test")) === ((0,
                                                                             "Test"))
    }

    "Make calls if required" in {
      timedRun("Make calls if required", batchAPI.sendRequest(1)) === ((1, 1))
    }

    "Batch calls if possible" in {
      val ids = List.range(1L, listMaxSize + 1)
      val reqs = ids
      val responses = ids

      import lib.instances.list._

      timedRun("Batch calls if possible", reqs.traverse(batchAPI.sendRequest)) === ((1,
                                                                                     responses))
    }

    "Don't blow the stack" in {
      val requests = List.range(1L, listMaxSize + 1)
      import lib.instances.list._

      timedRun("Don't blow the stack", requests.traverse(batchAPI.pure)) === ((0,
                                                                               requests))
    }

    "Respect FlatMap" in {
      val ids = List.range(1L, listMaxSize + 1)
      val reqs1 = ids.map(_ + 10)
      val reqs2 = ids.map(_ + 20)
      val responses1 = reqs1
      val responses2 = reqs2

      import lib.instances.list._

      val m: batchAPI.Batch[List[Response]] =
        for {
          r1 <- reqs1.traverse(batchAPI.sendRequest)
          r2 <- reqs2.traverse(batchAPI.sendRequest)
        } yield r1 ++ r2

      timedRun("Respect FlatMap", m) === ((2, responses1 ++ responses2))
    }

    "Respect NotFound" in {
      val ids = List.range(1, listMaxSize + 1)
      val reqsPos = ids
      val reqsNeg = ids.map(-_)
      val responses = ids

      import lib.instances.list._

      timedRun(
        "Respect NotFound",
        (reqsPos ++ reqsNeg)
          .traverse(batchAPI.sendRequest)) === ((listMaxSize * notFoundValue + 1,
                                                 responses ++ responses))
    }

    "Respect Ap" in {

      def fixture(i: Int): (List[Request], List[Response]) = {
        val ids = List.range(1, listMaxSize + 1)
        val l = ids.map(n => 100000000L * i + n)
        (l, l)
      }

      val (reqs1, resp1) = fixture(1)
      val (reqs2, resp2) = fixture(2)
      val (reqs3, resp3) = fixture(3)

      import lib.instances.list._

      val p1 = reqs1.traverse(batchAPI.sendRequest)
      val p2 = reqs2.traverse(batchAPI.sendRequest)
      val p3 = reqs3.traverse(batchAPI.sendRequest)

      val responses = (resp1, resp2, resp3)

      val viaFlatMap =
        for {
          l1 <- p1
          l2 <- p2
          l3 <- p3
        } yield (l1, l2, l3)

      val viaAp = {
        import batchAPI._
        import lib.syntax.applicative._
        type R = List[Response]
        pure((x: R) => (y: R) => (z: R) => (x, y, z)).ap(p1).ap(p2).ap(p3)
      }

      timedRun("Respect Ap: via flatMap", viaFlatMap) === ((3, responses))
      timedRun("Respect Ap: via ap", viaAp) === ((1, responses))
    }
  }
}
