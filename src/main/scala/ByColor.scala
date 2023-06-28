package chess

import cats.{ Applicative, Eq }
import cats.syntax.all.*
import scala.annotation.targetName

case class ByColor[A](white: A, black: A):

  def apply(color: Color) = if color.white then white else black

  def apply[B](color: Color)(f: A => B): B = if color.white then f(white) else f(black)

  def update(color: Color, f: A => A): ByColor[A] =
    if color.white then copy(white = f(white))
    else copy(black = f(black))

  def update(color: Color, f: A => Option[A]): Option[ByColor[A]] =
    if color.white then f(white).map(w => copy(white = w))
    else f(black).map(b => copy(black = b))

  def map[B](fw: A => B, fb: A => B) = copy(white = fw(white), black = fb(black))

  def map[B](f: A => B): ByColor[B]                 = map(f, f)
  def mapList[B](f: A => B): List[B]                = List(f(white), f(black))
  def mapReduce[B, C](f: A => B)(r: (B, B) => C): C = r(f(white), f(black))

  def mapWithColor[B](f: (Color, A) => B): ByColor[B] = ByColor(f(White, white), f(Black, black))

  def zip[B](other: ByColor[B]): ByColor[(A, B)] = ByColor((white, other.white), (black, other.black))
  def zip[B, C](other: ByColor[B], f: (A, B) => C): ByColor[C] =
    ByColor(f(white, other.white), f(black, other.black))
  def zipColor: ByColor[(Color, A)] = ByColor((White, white), (Black, black))
  def toPair: (A, A)                = (white, black)

  lazy val all: List[A] = List(white, black)

  def reduce[B](f: (A, A) => B): B = f(white, black)

  def fold[B](init: B)(f: (B, A) => B): B        = f(f(init, white), black)
  def fold[B](init: B)(f: (B, Color, A) => B): B = f(f(init, White, white), Black, black)

  def foreach[U](f: A => U): Unit =
    f(white)
    f(black)

  def forall(pred: A => Boolean) = pred(white) && pred(black)

  def exists(pred: A => Boolean) = pred(white) || pred(black)

  def flip: ByColor[A] = copy(white = black, black = white)

  def findColor(pred: A => Boolean): Option[Color] =
    if pred(white) then White.some
    else if pred(black) then Black.some
    else None

  def find(pred: A => Boolean): Option[A] =
    if pred(white) then white.some
    else if pred(black) then black.some
    else None

  @targetName("findOption")
  def find[B](f: A => Option[B]): Option[B] =
    f(white).orElse(f(black))

  def collect[B](f: PartialFunction[A, B]): Option[B] =
    f.lift(white).orElse(f.lift(black))

  def contains(a: A): Eq[A] ?=> Boolean =
    exists(_ === a)

  def flatMap[B](f: A => IterableOnce[B]): List[B] =
    all.flatMap(f)

  def traverse[F[_], B](f: A => F[B]): Applicative[F] ?=> F[ByColor[B]] =
    (f(white), f(black)).mapN(ByColor(_, _))

object ByColor:
  def apply[A](a: A): ByColor[A]          = ByColor(a, a)
  def apply[A](f: Color => A): ByColor[A] = ByColor(white = f(White), black = f(Black))

  given [A: Eq]: Eq[ByColor[A]] with
    def eqv(x: ByColor[A], y: ByColor[A]) =
      x.white === y.white && x.black === y.black

  extension [A](bc: ByColor[IterableOnce[A]])
    def flatten: List[A] = bc.all.flatten
