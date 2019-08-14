import de.bachelorarbeit.geneticminer.{EventLog, Individual}
import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable

class CompletenessTest extends FunSuite {
  test("getCompleteness.paper_ProDiGen_fig_4") {
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
    val result = individual.getCompleteness
    assertResult(1.0) {
      result
    }
  }

  test("getCompleteness.paper_ProDiGen_fig_5b") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val individual = new Individual(causalMatrix, eventLog, "x", "y")
    val result = individual.getCompleteness
    assertResult(1.0) {
      result
    }
  }

  test("getCompleteness.paper_ProDiGen_fig_5d") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("a", "b"))),
      "a" -> Edges(Places(Place("x")), Places(Place("d", "c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c", "e"))),
      "c" -> Edges(Places(Place("a"), Place("b")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d"), Place("e")), Places())
    )
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val individual = new Individual(causalMatrix, eventLog, "x", "y")
    val result = individual.getCompleteness
    result should be (-0.2 +- 0.0000001)
  }

  test("autokauf.1") {
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

    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"))

    val individual = new Individual(causalMatrix, eventLog, "a", "g")
    val result = individual.getCompleteness

    result should be (1)
  }

  test("autokauf.2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places())
    )

    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"))

    val individual = new Individual(causalMatrix, eventLog, "a", "g")
    val result = individual.getCompleteness

    result should be (0.8333 +- 0.0001)
  }

  test("getCompleteness.paper_ProDiGen_fig_4_mine") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b")), Places(Place("y"))),
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
    val result = individual.getCompleteness
    result should be (0.975 +- 0.00000001)
  }
}
