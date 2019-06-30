package autobatch

/** This monad transformer accumulate single-request calls to send them in batch*/
import cats.Monad
import scala.collection.immutable.{SortedSet, TreeMap}
import scala.collection.mutable
import scala.language.higherKinds

/** The configuration of the Batch Monad: the real batch API
  *
  * @tparam F the type of effects
  * @tparam Req the requests
  * @tparam Resp the response
  */
trait BatchAPI[F[_], Req, Resp] { self =>

  /** F is the base effect monad: Precepte, IO, etc */
  val baseMonad: Monad[F]

  /** We need to order requests! */
  val requestOrdering: Ordering[Req]

  /** The batch call. We send several request to it and expect several responses in return.
    *
    * /!\ VERY IMPORTANT: This call has to be ASSOCIATIVE, COMMUTATIVE AND IDEMPOTENT
    *                     for any individual requests!
    * Which means that:
    *
    * Property 1: calling sendBatch on List(r1, ..., rn) is exactly the same as
    *             calling sendBatch n times with each ri, in any order!
    * Property 2: calling sendBatch n times (n>=1) on the same input, produce
    *             the same effect as calling it only once!
    */
  def sendBatch(requests: List[Req]): F[List[Resp]]

  /** Regroup requests and responses
    *
    * INVARIANTS THAT MUST BE RESPECTED:
    *      let res = dispatch(reqs, resps)
    *
    *   1) for any r1, r2 requests in reqs
    *      if requestOrdering(r1, r2) == 0
    *      then res.get(r1) == resp.get(r2)
    *
    *   2) any request in res is a request in reqs
    *      i.e. res.keys is a subset of reqs.toSet
    */
  def dispatch(requests: List[Req], response: List[Resp]): Map[Req, Resp]

  /** What to do if the dispatch gave no response for this request? */
  def notFound(request: Req): F[Resp]

  final type Batch[A] = BatchT[F, Req, Resp, A]

  final object implicits {
    implicit final val baseMonadImplicit: Monad[F] = baseMonad
    implicit final val requestOrderingImplicit: Ordering[Req] = requestOrdering
    implicit final val batchMonad: Monad[Batch] =
      BatchT.batchTMonadInstance[F, Req, Resp]
  }

  /** INVARIANTS THAT ARE RESPECTED:
    *     let res  = dispatch(reqs, resps)
    *         tres = totalDispatch(resq, resp)
    *
    *  1) for any r1, r2 requests in reqs
    *     if requestOrdering(r1, r2) == 0
    *     then tres.get(r1) == tres.get(r2)
    *                       == Some(res.get(r1))
    *                       == Some(res.get(r2))
    *
    *  2) reqs.keys == reqs.toSet
    */
  @inline final def totalDispatch(
      requests: List[Req],
      response: List[Resp]): Map[Req, Option[Resp]] = {
    val res = dispatch(requests, response)
    TreeMap.empty(requestOrdering) ++ requests.map(r => r -> res.get(r))
  }

  @inline final def pure[A](a: A): Batch[A] =
    BatchT.pure(a)

  @inline final def sendRequest(request: Req): Batch[Resp] =
    BatchT.sendRequest(request)

  @inline final def ap[A, B](ff: Batch[A => B])(fa: Batch[A]): Batch[B] =
    BatchT.ap(ff)(fa)

  @inline final def map[A, B](fa: Batch[A])(f: A => B): Batch[B] =
    BatchT.map(fa)(f)

  @inline final def flat[A](value: Batch[Batch[A]]): Batch[A] =
    BatchT.flatten(value)

  @inline final def flatMap[A, B](fa: Batch[A])(f: A => Batch[B]): Batch[B] =
    BatchT.flatMap(fa)(f)

  @inline final def tailRecM[A, B](a: A)(
      f: A => Batch[Either[A, B]]): Batch[B] =
    BatchT.tailRecM(a)(f)

  def contraMap[Req2](f: Req2 => Req): BatchAPI[F, Req2, Resp] =
    new BatchAPI[F, Req2, Resp] {
      val baseMonad: Monad[F] = self.baseMonad
      val requestOrdering: Ordering[Req2] = self.requestOrdering.on[Req2](f)

      @inline final def toReq(l: List[Req2]): List[Req] =
        (SortedSet.empty[Req](self.requestOrdering) ++ l.map(f)).toList

      final def sendBatch(requests: List[Req2]): F[List[Resp]] =
        self.sendBatch(toReq(requests))

      final def dispatch(requests: List[Req2],
                         response: List[Resp]): Map[Req2, Resp] = {
        val tr: mutable.Builder[(Req2, Resp), TreeMap[Req2, Resp]] =
          TreeMap.newBuilder[Req2, Resp](requestOrdering)

        val dp: Map[Req, Resp] = self.dispatch(toReq(requests), response)

        for (req2 <- requests)
          dp.get(f(req2)) match {
            case Some(resp) => tr += (req2 -> resp)
            case _          => ()
          }

        tr.result()
      }

      def notFound(request: Req2): F[Resp] = self.notFound(f(request))
    }
}
