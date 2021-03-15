package lab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

object StreamsTests {

    import u03.Lists.List._
    import u03.Streams.Stream._

    @Test
    def testDrop() = {
        val stream = take(iterate(0)(_ + 1))(10)
        val expected = Cons(6, Cons(7, Cons(8, Cons(9, Nil()))))
        assertEquals(expected, toList(drop(stream)(6)))
    }

    @Test
    def testConstant() = {
        val expected = Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil())))))
        assertEquals(expected, toList(take(constant("x"))(5)))
    }
}
