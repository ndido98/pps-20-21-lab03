package lab

object Lists {

    object List {
        import u02.Optionals.Option
        import u02.Optionals.Option._
        import u03.Lists._
        import u03.Lists.List._

        def map[A,B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(x => Cons(mapper(x), Nil()))

        def filter[A](l: List[A])(predicate: A => Boolean): List[A] =
            flatMap(l)(x => if (predicate(x)) Cons(x, Nil()) else Nil())

        def drop[A](l: List[A], count: Int): List[A] = l match {
            case Cons(_, t) if count > 0 => drop(t, count - 1)
            case _ => l
        }

        def flatMap[A, B](l: List[A])(flatMapper: A => List[B]): List[B] = l match {
            case Cons(h, t) => append(flatMapper(h), flatMap(t)(flatMapper))
            case Nil() => Nil()
        }

        def max(l: List[Int]): Option[Int] = l match {
            case Cons(h, t) => Some(Math.max(h, getOrElse(max(t), Int.MinValue)))
            case Nil() => None()
        }
    }
}