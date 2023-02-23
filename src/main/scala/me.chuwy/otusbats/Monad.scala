package me.chuwy.otusbats

import scala.concurrent.Future

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A]
}

object Monad {
  implicit val optionMonad: Monad[Option] = new Monad[Option] {

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = flatten(fa.map(a => f(a)))

    override def point[A](a: A): Option[A] = Some(a)

    override def flatten[A](fa: Option[Option[A]]): Option[A] = fa.flatten

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
}

implicit val listMonad: Monad[List] = new Monad[List] {

  override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = flatten(fa.map(a => f(a)))

  override def point[A](a: A): List[A] = List(a)

  override def flatten[A](fa: List[List[A]]): List[A] = fa.flatten

  override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
}

implicit val futureMonad: Monad[Future] = new Monad[Future] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = flatten(fa.map(a => f(a)))

  override def point[A](a: A): Future[A] = Future(a)

  override def flatten[A](fa: Future[Future[A]]): Future[A] = fa.flatten

  override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(a => f(a))
}
}
