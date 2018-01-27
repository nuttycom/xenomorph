package xenomorph.scodec

import scodec.{Attempt, Decoder}

import scalaz.{Applicative, Monad, Traverse}

trait LowPriorityScalazInstances {

  implicit val AttemptInstance: Monad[Attempt] with Traverse[Attempt] = new Monad[Attempt] with Traverse[Attempt]{
    def point[A](a: => A) = Attempt.successful(a)
    def bind[A, B](fa: Attempt[A])(f: A => Attempt[B]) = fa.flatMap(f)

    def traverseImpl[G[_], A, B](fa: Attempt[A])(f: A => G[B])(implicit G: Applicative[G]): G[Attempt[B]] =
      fa match {
        case Attempt.Successful(value)  => G.map(f(value))(Attempt.Successful(_))
        case failure@Attempt.Failure(_) => G.point(failure)
      }
  }

  implicit val DecoderInstance : Monad[Decoder] = new Monad[Decoder]{
    def point[A](a: => A) = Decoder.point(a)
    def bind[A, B](fa: Decoder[A])(f: A => Decoder[B]) = fa.flatMap(f)
  }

}
