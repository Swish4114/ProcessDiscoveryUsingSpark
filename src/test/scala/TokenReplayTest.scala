import de.bachelorarbeit.geneticminer.{Edges, EventLog, Individual, Place, Places, Task, Trace}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.mutable


class TokenReplayTest extends FunSuite{

  private def convertToTripleTuple(tokenReplayResult: (List[Task], List[Task], List[Task], Int)): (Int, Int, Int) = {
    (tokenReplayResult._1.size, tokenReplayResult._2.size, tokenReplayResult._3.size)
  }


  test("tokenReplay.autokauf_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )

    val trace = Trace("a", "b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((7, 0, 0)) {
      result
    }
  }

  test("tokenReplay.autokauf_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )

    val trace = Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((11, 0, 0)) {
      result
    }
  }

  test("tokenReplay.autokauf_3") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )
    val trace = Trace("a", "b", "c", "d", "e", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((8, 2, 1)) {
      result
    }
  }

  test("tokenReplay.autokauf_4") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(Place("i")), Places(Place("b", "i"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "i" -> Edges(Places(Place("a")), Places(Place("a")))
    )
    val trace = Trace("a", "b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((7, 0, 0)) {
      result
    }
  }

  test("tokenReplay.autokauf_5") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(Place("i")), Places(Place("b", "i"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "i" -> Edges(Places(Place("a")), Places(Place("a")))
    )
    val trace = Trace("b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((5, 1, 1)) {
      result
    }
  }

  test("tokenReplay.autokauf_6") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(Place("i")), Places(Place("i", "b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "i" -> Edges(Places(Place("a")), Places(Place("a")))
    )
    val trace = Trace("a", "i", "a", "i", "b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((9, 1, 1)) {
      result
    }
  }

  test("tokenReplay.autokauf_7") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(Place("i")), Places(Place("i", "b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("h", "f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "i" -> Edges(Places(Place("a")), Places(Place("a")))
    )
    val trace = Trace("a", "i", "a", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((6, 2, 1)) {
      result
    }
  }

  test("tokenReplay.process_mining_in_a_large_a_tutorial_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("e", "b"), Place("c", "e"))),
      "b" -> Edges(Places(Place("a")), Places(Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("e", "b"), Place("c", "e")), Places()),
      "e" -> Edges(Places(Place("a"), Place("a")), Places(Place("d"), Place("d")))
    )
    val trace = Trace("a", "b", "c", "d")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "d")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((4, 0, 0)) {
      result
    }
  }

  test("tokenReplay.process_mining_in_a_large_a_tutorial_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b", "e"), Place("c", "e"))),
      "b" -> Edges(Places(Place("a")), Places(Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("b", "e"), Place("c", "e")), Places()),
      "e" -> Edges(Places(Place("a"), Place("a")), Places(Place("d"), Place("d")))
    )
    val trace = Trace("a", "d")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "d")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((1, 2, 2)) {
      result
    }
  }

  test("tokenReplay.buch_s_197_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b", "c"), Place("d"))),
      "b" -> Edges(Places(Place("a")), Places(Place("e"))),
      "c" -> Edges(Places(Place("a")), Places(Place("e"))),
      "d" -> Edges(Places(Place("a")), Places(Place("e"))),
      "e" -> Edges(Places(Place("b", "c"), Place("d")), Places(Place("h", "g", "f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("b", "c"), Place("d"))),
      "g" -> Edges(Places(Place("e")), Places(Place("x"))),
      "h" -> Edges(Places(Place("e")), Places(Place("x"))),
      "x" -> Edges(Places(Place("g", "h")), Places())
    )
    val trace = Trace("a", "c", "d", "e", "h", "x")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "x")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((6, 0, 0)) {
      result
    }
  }

  test("tokenReplay.buch_s_197_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b", "c"), Place("d"))),
      "b" -> Edges(Places(Place("a")), Places(Place("e"))),
      "c" -> Edges(Places(Place("a")), Places(Place("e"))),
      "d" -> Edges(Places(Place("a")), Places(Place("e"))),
      "e" -> Edges(Places(Place("b", "c"), Place("d")), Places(Place("f", "g", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("b", "c"), Place("d"))),
      "g" -> Edges(Places(Place("e")), Places(Place("x"))),
      "h" -> Edges(Places(Place("e")), Places(Place("x"))),
      "x" -> Edges(Places(Place("g", "h")), Places())
    )
    val trace = Trace("a", "c", "d", "e")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "x")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((4, 1, 1)) {
      result
    }
  }

  test("tokenReplay.buch_s_199_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("c", "b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("c", "b")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d")), Places(Place("g", "f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("c", "b"))),
      "g" -> Edges(Places(Place("e")), Places(Place("x"))),
      "h" -> Edges(Places(Place("e")), Places(Place("x"))),
      "x" -> Edges(Places(Place("g", "h")), Places())
    )
    val trace = Trace("a", "d", "c", "e", "h", "x")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "x")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((5, 1, 1)) {
      result
    }
  }

  test("tokenReplay.buch_s_199_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("c", "b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("c", "b")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d")), Places(Place("h", "g", "f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("c", "b"))),
      "g" -> Edges(Places(Place("e")), Places(Place("x"))),
      "h" -> Edges(Places(Place("e")), Places(Place("x"))),
      "x" -> Edges(Places(Place("g", "h")), Places())
    )
    val trace = Trace("a", "c", "d", "e", "h", "x")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "x")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((6, 0, 0)) {
      result
    }
  }

  test("tokenReplay.buch_s_200") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("e"))),
      "d" -> Edges(Places(Place("a")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c"), Place("d")), Places(Place("h"))),
      "h" -> Edges(Places(Place("e")), Places())
    )
    val trace = Trace("a", "d", "e")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "h")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((2, 2, 2)) {
      result
    }
  }

  test("tokenReplay.incorrect_petri_nets_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b")), Places()),
      "d" -> Edges(Places(Place("b")), Places()),
      "e" -> Edges(Places(), Places(Place("f"), Place("h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places())
    )
    val trace = Trace("a", "b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((6, 1, 3)) {
      result
    }
  }

  test("tokenReplay.incorrect_petri_nets_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b")), Places(Place("e"))),
      "d" -> Edges(Places(), Places()),
      "e" -> Edges(Places(Place("c")), Places(Place("f"))),
      "f" -> Edges(Places(Place("e")), Places()),
      "g" -> Edges(Places(), Places()),
      "h" -> Edges(Places(), Places())
    )
    val trace = Trace("a", "b", "c", "e", "f")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "f")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((5, 0, 0)) {
      result
    }
  }

  test("tokenReplay.incorrect_petri_nets_3") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b")), Places(Place("e"))),
      "d" -> Edges(Places(), Places()),
      "e" -> Edges(Places(Place("c")), Places(Place("f"))),
      "f" -> Edges(Places(Place("e")), Places()),
      "g" -> Edges(Places(), Places()),
      "h" -> Edges(Places(), Places())
    )
    val trace = Trace("a", "b", "c", "d", "e", "f")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "f")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((5, 1, 1)) {
      result
    }
  }

  test("tokenReplay.incorrect_petri_nets_4") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b")), Places(Place("e"))),
      "d" -> Edges(Places(), Places()),
      "e" -> Edges(Places(Place("c")), Places(Place("f"))),
      "f" -> Edges(Places(Place("e")), Places()),
      "g" -> Edges(Places(), Places()),
      "h" -> Edges(Places(), Places())
    )
    val trace = Trace("a", "g", "b", "c", "d", "e", "f")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "f")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((5, 2, 2)) {
      result
    }
  }

  test("tokenReplay.autokauf_8") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("h"), Place("b")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b"), Place("h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c"), Place("d")), Places(Place("h"), Place("f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("d"), Place("c")))
    )
    val trace = Trace("a", "b", "d", "c", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((5, 2, 1)) {
      result
    }
  }

  test("tokenReplay.autokauf_9") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("h"), Place("b")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b"), Place("h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c"), Place("d")), Places(Place("h"), Place("f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("d"), Place("c")))
    )
    val trace = Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    val result = convertToTripleTuple(individual.tokenReplay(trace))
    assertResult((7, 4, 2)) {
      result
    }
  }

  test("tokenReplay.BachelorthesisTokenReplay.1") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val individual = new Individual(causalMatrix, eventLog, "x", "y")
    val result = convertToTripleTuple(individual.tokenReplay(Trace("x", "a", "c", "d", "y")))
    assertResult((5, 0, 0)) {
      result
    }
  }

  test("tokenReplay.BachelorthesisTokenReplay.2") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val individual = new Individual(causalMatrix, eventLog, "x", "y")
    val result = convertToTripleTuple(individual.tokenReplay(Trace("x", "b", "c", "d", "y")))

    assertResult((4, 1, 1)) {
      result
    }
  }

  test("tokenReplay.BachelorthesisTokenReplay.2.2") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(Place("b")), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e", "x"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val individual = new Individual(causalMatrix, eventLog, "x", "y")
    val result = convertToTripleTuple(individual.tokenReplay(Trace("x", "b", "c", "x", "b", "c", "d", "y")))
    //println(individual.tokenReplay(Trace("x", "b", "c", "d", "y")))

    assertResult((7, 1, 2)) {
      result
    }
  }

  test("weirdCausalMatrix") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"), Place("c"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("h", "a", "b")), Places(Place("e"))),
      "d" -> Edges(Places(Place("h", "b")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c", "d")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )
    val autoKauf = EventLog(
      Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "d", "c", "e", "h", "d", "c", "e", "f", "g"))

    val individual = new Individual(causalMatrix, autoKauf, "a", "g")
    //individual.calculateFitnessValues()
    var result = convertToTripleTuple(individual.tokenReplay(autoKauf.head))
    result should be (7, 0, 2)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(1)))
    result should be (7, 0, 2)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(2)))
    result should be (11, 0, 3)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(3)))
    result should be (11, 0, 3)

    val punishment = 10 / (4 - 4 + 1).toDouble
    val completeness = (36 - punishment) / 36.0
    individual.getCompleteness should be (completeness)
    individual.getPrecision should be (1/(2 * 10 + 2 * 16).toDouble)
    individual.getSimplicity should be (1/22.0)
  }

  test("weirdCausalMatrix2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"), Place("c"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("h", "a", "b")), Places(Place("e", "x"))),
      "d" -> Edges(Places(Place("h", "b")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c", "d")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "x" -> Edges(Places(Place("c")), Places())
    )
    val autoKauf = EventLog(
      Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "d", "c", "e", "h", "d", "c", "e", "f", "g"))

    val individual = new Individual(causalMatrix, autoKauf, "a", "g")
    //individual.calculateFitnessValues()
    var result = convertToTripleTuple(individual.tokenReplay(autoKauf.head))
    result should be (7, 0, 2)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(1)))
    result should be (7, 0, 2)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(2)))
    result should be (11, 0, 3)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(3)))
    result should be (11, 0, 3)

    val punishment = 10 / (4 - 4 + 1).toDouble
    val completeness = (36 - punishment) / 36.0
    individual.getCompleteness should be (completeness)
    individual.getPrecision should be (1/(2 * 11 + 2 * 18).toDouble)
    individual.getSimplicity should be (1/24.0)
  }

  test("weirdCausalMatrix3") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"), Place("c", "x"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c", "x"), Place("d"))),
      "c" -> Edges(Places(Place("h", "a", "b")), Places(Place("e"))),
      "d" -> Edges(Places(Place("h", "b")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c", "d")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c", "x"), Place("d"))),
      "x" -> Edges(Places(Place("h", "a", "b")), Places())
    )
    val autoKauf = EventLog(
      Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "d", "c", "e", "h", "d", "c", "e", "f", "g"))

    val individual = new Individual(causalMatrix, autoKauf, "a", "g")
    //individual.calculateFitnessValues()
    var result = convertToTripleTuple(individual.tokenReplay(autoKauf.head))
    result should be (7, 0, 2)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(1)))
    result should be (7, 0, 2)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(2)))
    result should be (11, 0, 3)

    result = convertToTripleTuple(individual.tokenReplay(autoKauf(3)))
    result should be (11, 0, 3)

    val punishment = 10 / (4 - 4 + 1).toDouble
    val completeness = (36 - punishment) / 36.0
    individual.getCompleteness should be (completeness)
    individual.getPrecision should be (1/(2 * 12 + 2 * 19).toDouble)
    individual.getSimplicity should be (1/28.0)
  }
}
