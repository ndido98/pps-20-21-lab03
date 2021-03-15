package lab

object Lists {

    // A generic linked list
    sealed trait List[E]

    // a companion object (i.e., module) for List
    object List {
        case class Cons[E](head: E, tail: List[E]) extends List[E]
        case class Nil[E]() extends List[E]

        def sum(l: List[Int]): Int = l match {
            case Cons(h, t) => h + sum(t)
            case _ => 0
        }

        def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
            case (Cons(h, t), l2) => Cons(h, append(t, l2))
            case _ => l2
        }

        def map[A,B](l: List[A])(mapper: A=>B): List[B] = l match {
            case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
            case Nil() => Nil()
        }

        def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
            case Cons(h,t) if (pred(h)) => Cons(h, filter(t)(pred))
            case Cons(_,t) => filter(t)(pred)
            case Nil() => Nil()
        }

        def drop[A](l: List[A], count: Int): List[A] = l match {
            case Cons(_, t) if count > 0 => drop(t, count - 1)
            case Cons(h, t) => Cons(h, t)
            case Nil() => Nil()
        }

        def flatMap[A, B](l: List[A])(flatMapper: A => List[B]): List[B] = l match {
            case Cons(h, t) => append(flatMapper(h), flatMap(t)(flatMapper))
            case Nil() => Nil()
        }
    }
}