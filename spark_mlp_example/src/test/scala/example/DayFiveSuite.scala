package limmen.kth.se

import org.scalatest._

/**
 * Test suite for DayFive.scala
 *
 */
class DayFiveSuite extends FunSuite with Matchers {
  test("dayFive method") {
    val stream = new java.io.ByteArrayOutputStream()
    val dayFiveCorrect = new DayFive() with MockCorrectInput
    val dayFiveWrong = new DayFive() with MockWrongInput
    assert(dayFiveCorrect.dayFive())
    assert(!dayFiveWrong.dayFive())
    assert(dayFiveCorrect.getPw().equals("test"))
    assert(dayFiveCorrect.getUsername().equals("test"))
  }

  trait MockCorrectInput extends Input {
    override def nextLine(): String = {
      return "test"
    }
  }

  trait MockWrongInput extends Input {
    override def nextLine(): String = {
      return "wrong"
    }
  }
}
