package autobatch

/** This monad transformer accumulate single-request calls to send them in batch*/
import cats.Monad
import cats.syntax.flatMap._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/** Presently, if there is an error, in one item of the batch, all fail.
  * Possible evolution: make BatchT an error monad so that it can recover
  * from failure on a per item basis.
  */
sealed abstract class BatchT[F[_], Req, Resp, A](val hasRequests: Boolean) {
  @inline final def run(batchAPI: BatchAPI[F, Req, Resp]): F[A] =
    BatchT.run(batchAPI, this)

  @inline final def requests(
      implicit requestOrdering: Ordering[Req]
  ): SortedSet[Req] =
    BatchT.requests(this)(requestOrdering)
}

object BatchT {

  ///////////////////////////////////
  // Public Constructors of BatchT

  /** Send a non-batched request */
  @inline def sendRequest[F[_], Req, Resp](
      request: Req
  ): BatchT[F, Req, Resp, Resp] =
    BatchT.SendRequest(request)

  /** Applicative/Monadic pure */
  @inline def pure[F[_], Req, Resp, A](x: A): BatchT[F, Req, Resp, A] = Pure(x)

  /** Applicative ap */
  @inline def ap[F[_], Req, Resp, A, B](
      ff: BatchT[F, Req, Resp, A => B]
  )(fa: BatchT[F, Req, Resp, A]): BatchT[F, Req, Resp, B] =
    (ff, fa) match {
      case (Pure(f), Pure(a)) =>
        Pure(f(a))
      case (_, _) =>
        Ap(ff, fa)
    }

  /** Functorial map */
  @inline def map[F[_], Req, Resp, A, B](
      fa: BatchT[F, Req, Resp, A]
  )(f: A => B): BatchT[F, Req, Resp, B] =
    ap[F, Req, Resp, A, B](pure(f))(fa)

  /** Monadic flatten */
  @inline def flatten[F[_], Req, Resp, A](
      value: BatchT[F, Req, Resp, BatchT[F, Req, Resp, A]]
  ): BatchT[F, Req, Resp, A] =
    value match {
      case Pure(m) => m
      case _       => Flatten(value)
    }

  /** Monadic flatMap */
  @inline def flatMap[F[_], Req, Resp, A, B](
      fa: BatchT[F, Req, Resp, A]
  )(f: A => BatchT[F, Req, Resp, B]): BatchT[F, Req, Resp, B] =
    fa match {
      case Pure(a) => f(a)
      case _       => flatten(map(fa)(f))
    }

  /** Cats tailrecM */
  @inline def tailRecM[F[_], Req, Resp, A, B](
      a: A
  )(f: A => BatchT[F, Req, Resp, Either[A, B]]): BatchT[F, Req, Resp, B] = {
    val m = f(a)
    flatMap(m) {
      case Left(a2) => tailRecM(a2)(f)
      case Right(b) => pure[F, Req, Resp, B](b)
    }
  }

  ///////////////////////////////////
  // Private Constructors of BatchT

  private final case class Pure[F[_], Req, Resp, A](value: A)
      extends BatchT[F, Req, Resp, A](false)

  private final case class Lift[F[_], Req, Resp, A](value: F[A])
      extends BatchT[F, Req, Resp, A](false)

  private final case class Ap[F[_], Req, Resp, A, B](
      fun: BatchT[F, Req, Resp, A => B],
      arg: BatchT[F, Req, Resp, A]
  ) extends BatchT[F, Req, Resp, B](fun.hasRequests || arg.hasRequests)

  private final case class Flatten[F[_], Req, Resp, A](
      value: BatchT[F, Req, Resp, BatchT[F, Req, Resp, A]]
  ) extends BatchT[F, Req, Resp, A](value.hasRequests)

  private final case class SendRequest[F[_], Req, Resp](request: Req)
      extends BatchT[F, Req, Resp, Resp](true)

  ///////////////////////////////////
  // Running BatchT

  /** Collect all the request inside a {{{BatchT[F, Req, Resp, A]}}} */
  def requests[F[_], Req, Resp, A](
      fa: BatchT[F, Req, Resp, A]
  )(implicit requestOrdering: Ordering[Req]): SortedSet[Req] = {
    type G[X] = BatchT[F, Req, Resp, X]
    val reqs = collection.immutable.TreeSet.newBuilder[Req](requestOrdering)

    @tailrec
    def aux(stack: List[G[_]]): Unit =
      stack match {
        case hd :: tl =>
          if (hd.hasRequests) hd match {
            case sr: SendRequest[F, Req, Resp] =>
              reqs += sr.request
              aux(tl)
            case Ap(fun, arg) => aux(fun :: arg :: tl)
            case Flatten(ffa) => aux(ffa :: tl)
            case _            => ()
          } else aux(tl)
        case _ => ()
      }

    aux(List(fa))
    reqs.result()
  }

  /** Run the BatchT monad */
  def run[F[_], Req, Resp, A](
      batchAPI: BatchAPI[F, Req, Resp],
      arg: BatchT[F, Req, Resp, A]
  ): F[A] = {
    type G[X] = BatchT[F, Req, Resp, X]

    /* If you modify this, check carefully that replace normalize term and terminates
     *
     * HYPOTHESIS:
     *   1) `fa.requests` is non empty
     *   2) each `fa.request` is a key in `answers` (see totalDispatch)
     *   2) if `fa` terminates, so does `replace(answers)(fa)`
     */
    def replace[B](
        answers: Map[Req, Option[Resp]],
        argReplace: BatchT[F, Req, Resp, B]
    ): BatchT[F, Req, Resp, B] = {
      sealed abstract class Cont[C, D]
      final case class Id[D]() extends Cont[D, D]
      final case class Ap1[X, C, D](apArg: G[X], k: Cont[C, D])
          extends Cont[X => C, D]
      final case class Ap2[X, C, D](normFun: G[X => C], k: Cont[C, D])
          extends Cont[X, D]
      final case class Flat1[X, C, D](k: Cont[C, D])(
          implicit val ev: G[X] =:= G[G[C]]
      ) extends Cont[X, D]

      final case class AuxArgs[C, D](fc: G[C], k: Cont[C, D])

      @tailrec
      def runCont[C, D](k: Cont[C, D], fc: G[C]): Either[AuxArgs[_, D], G[D]] =
        k match {
          case _: Id[d] => Right(fc)
          case Ap1(apArg, k2) =>
            Left(AuxArgs(apArg, Ap2(fc, k2))) // arg = normFun
          case Ap2(normFun, k2) => runCont(k2, ap(normFun)(fc))
          case v: Flat1[c, a, d] =>
            runCont[a, d](v.k, flatten(v.ev(fc)))
        }

      @tailrec
      def aux[C, D](fc: G[C], k: Cont[C, D]): G[D] =
        fc match {
          case sr: SendRequest[F, Req, Resp] =>
            val ret: G[Resp] =
              answers.get(sr.request) match {
                case Some(Some(a)) =>
                  Pure(a) // no request, so (1) OK. Normal
                case Some(None) =>
                  Lift(batchAPI.notFound(sr.request)) // no request, so OK. Normal
                case _ =>
                  fc // Not in answers domain so OK. Normal
              }
            runCont(k, ret) match {
              case Right(x)                  => x
              case Left(args: AuxArgs[c, d]) => aux(args.fc, args.k)
            }

          case Ap(fun, arg) =>
            aux(fun, Ap1(arg, k))

          case Flatten(ffa) =>
            aux(ffa, Flat1(k))

          case _ =>
            runCont(k, fc) match {
              case Right(x)                  => x
              case Left(args: AuxArgs[c, d]) => aux(args.fc, args.k)
            }
        }

      aux(argReplace, Id())
    }

    import batchAPI.implicits._

    def eval[B](fb: G[B]): F[B] = {
      sealed abstract class Cont[C, D]
      final case class Id[D]() extends Cont[D, D]
      final case class Ap1[X, C, D](apArg: G[X], k: Cont[C, D])
          extends Cont[X => C, D]
      final case class Ap2[X, C, D](normFun: F[X => C], k: Cont[C, D])
          extends Cont[X, D]
      final case class Flat1[X, C, D](k: Cont[C, D])(
          implicit val ev: F[X] =:= F[G[C]]
      ) extends Cont[X, D]

      final case class AuxArgs[C, D](fc: G[C], k: Cont[C, D])

      @tailrec
      def runCont[C, D](k: Cont[C, D], arg: F[C]): Either[AuxArgs[_, D], F[D]] =
        k match {
          case _: Id[d]       => Right(arg)
          case Ap1(apArg, k2) => Left(AuxArgs(apArg, Ap2(arg, k2)))
          case Ap2(ioFun, k2) => runCont(k2, batchAPI.baseMonad.ap(ioFun)(arg))
          case x: Flat1[c, a, d] =>
            runCont[a, d](x.k, x.ev(arg).flatMap(eval))
        }

      @inline def auxTrick[C, D](fc: G[C], k: Cont[C, D]): F[D] =
        aux(fc, k)

      @tailrec
      def aux[C, D](fc: G[C], k: Cont[C, D]): F[D] =
        if (fc.hasRequests) {
          val requests = fc.requests(batchAPI.requestOrdering).toList
          batchAPI.sendBatch(requests).flatMap { (responses: List[Resp]) =>
            val replaced =
              replace(batchAPI.totalDispatch(requests, responses), fc)
            auxTrick(replaced, k)
          }
        } else
          fc match { // SO fa.requests IS EMPTY
            case Pure(a) =>
              runCont(k, batchAPI.baseMonad.pure(a)) match {
                case Right(x)                  => x
                case Left(args: AuxArgs[c, d]) => aux(args.fc, args.k)
              }
            case Lift(effect) =>
              runCont(k, effect) match {
                case Right(x)                  => x
                case Left(args: AuxArgs[c, d]) => aux(args.fc, args.k)
              }
            case _: SendRequest[f, req, resp] =>
              assert(false, "Impossible because fa.requests is empty")
            case Ap(fun, arg) => aux(fun, Ap1(arg, k))
            case Flatten(ffa) => aux(ffa, Flat1(k))
          }

      aux(fb, Id())
    }

    eval(arg)
  }

  ///////////////////////////////////
  // Implicits

  implicit def batchTMonadInstance[F[_], Req, Resp]
      : Monad[BatchT[F, Req, Resp, ?]] =
    new Monad[BatchT[F, Req, Resp, ?]] {
      @inline final def pure[A](a: A): BatchT[F, Req, Resp, A] =
        BatchT.pure(a)

      @inline override final def ap[A, B](
          ff: BatchT[F, Req, Resp, A => B]
      )(fa: BatchT[F, Req, Resp, A]): BatchT[F, Req, Resp, B] =
        BatchT.ap(ff)(fa)

      @inline override final def map[A, B](
          fa: BatchT[F, Req, Resp, A]
      )(f: A => B): BatchT[F, Req, Resp, B] =
        BatchT.map(fa)(f)

      @inline final def flatMap[A, B](
          fa: BatchT[F, Req, Resp, A]
      )(f: A => BatchT[F, Req, Resp, B]): BatchT[F, Req, Resp, B] =
        BatchT.flatMap(fa)(f)

      @inline final def tailRecM[A, B](
          a: A
      )(f: A => BatchT[F, Req, Resp, Either[A, B]]): BatchT[F, Req, Resp, B] =
        BatchT.tailRecM(a)(f)
    }
}
