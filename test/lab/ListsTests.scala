package lab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

object ListsTests {
    import u03.Lists.List.Cons
    import u03.Lists.List.Nil
    import u03.Lists.List.sum
    import u03.Lists.List.append
    import lab.Lists.List._

    val testList = Cons(1, Cons(2, Cons(3, Nil())))

    @Test
    def testSum() = {
        assertEquals(6, sum(testList))
        assertEquals(0, sum(Nil()))
    }

    @Test
    def testAppend() = {
        val appendedList = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
        assertEquals(appendedList, append(testList, Cons(4, Nil())))
    }

    @Test
    def testMap() = {
        val mapped = Cons(10, Cons(20, Cons(30, Nil())))
        assertEquals(mapped, map(testList)(_ * 10))
        assertEquals(Nil[Int](), map(Nil[Int]())(x => x))
    }

    @Test
    def testFilter() = {
        val odd = Cons(1, Cons(3, Nil()))
        assertEquals(odd, filter(testList)(_ % 2 == 1))
    }

    @Test
    def testDrop() = {
        val oneDropped = Cons(2, Cons(3, Nil()))
        val twoDropped = Cons(3, Nil())
        val moreThanLengthDropped = Nil[Int]()
        assertEquals(oneDropped, drop(testList, 1))
        assertEquals(twoDropped, drop(testList, 2))
        assertEquals(moreThanLengthDropped, drop(testList, 5))
    }

    @Test
    def testFlatMap() = {
        val plusOne = Cons(2, Cons(3, Cons(4, Nil())))
        val plusOnePlusTwo = Cons(2, Cons(3, Cons(3, Cons(4, Cons(4, Cons(5, Nil()))))))
        assertEquals(plusOne, flatMap(testList)(x => Cons(x + 1, Nil())))
        assertEquals(plusOnePlusTwo, flatMap(testList)(x => Cons(x + 1, Cons(x + 2, Nil()))))
    }

    @Test
    def testMax() = {
        import u02.Optionals.Option._
        assertEquals(Some(3), max(testList))
        assertEquals(None(), max(Nil()))
    }
}
