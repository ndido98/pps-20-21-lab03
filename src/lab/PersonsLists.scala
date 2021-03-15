package lab

object PersonsLists {
    import u03.Lists._
    import u03.Lists.List.Cons
    import u03.Lists.List.Nil
    import u02.Modules._
    import u02.Modules.Person._
    import lab.Lists.List._

    def getCourses(l: List[Person]): List[String] = flatMap(l)({
        case Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
    })
}
