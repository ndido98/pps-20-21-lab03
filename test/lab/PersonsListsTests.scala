package lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

object PersonsListsTests {
    import u02.Modules._
    import u02.Modules.Person._
    import u03.Lists._
    import u03.Lists.List.append
    import u03.Lists.List.Cons
    import u03.Lists.List.Nil
    import lab.PersonsLists._

    val students: List[Person] = Cons(Student("Alice", 1998), Cons(Student("Bob", 1999), Nil()))
    val teachers: List[Person] = Cons(Teacher("Carl", "Programming 101"), Cons(Teacher("Dean", "Data Structures"), Nil()))
    val persons: List[Person] = append(students, teachers)

    @Test
    def testGetCourses() = {
        assertEquals(Cons("Programming 101", Cons("Data Structures", Nil())), getCourses(persons))
        assertEquals(Nil(), getCourses(students))
        assertEquals(Nil(), getCourses(Nil()))
    }
}
