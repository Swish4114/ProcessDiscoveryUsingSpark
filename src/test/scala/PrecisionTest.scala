import de.bachelorarbeit.geneticminer.{EventLog, Individual}
import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable

class PrecisionTest extends FunSuite {

  test("getPrecision.autokauf_1") {
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
    individual.getPrecision should be (1/8.0 +- 0.000001)
  }

  test("getPrecision.autokauf_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("h", "f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )
    val trace = Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    individual.getPrecision should be (1/13.0 +- 0.000001)
  }

  test("getPrecision.autokauf_3") {
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
    individual.getPrecision should be (1/11.0 +- 0.000001)
  }

  test("getPrecision.autokauf_4") {
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
    val trace = Trace("a", "b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    individual.getPrecision should be (1/9.0 +- 0.000001)
  }

  test("getPrecision.autokauf_5") {
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
    individual.getPrecision should be (1/7.0 +- 0.000001)
  }

  test("getPrecision.autokauf_6") {
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
    val trace = Trace("a", "i", "b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    individual.getPrecision should be (1/10.0 +- 0.000001)
  }

  test("getPrecision.autokauf_7") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(Place("i")), Places(Place("b", "i"))),
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
    individual.getPrecision should be (1/10.0 +- 0.000001)
  }

  test("getAllEnabledActivities.process_mining_in_a_large_a_tutorial_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b", "e"), Place("e", "c"))),
      "b" -> Edges(Places(Place("a")), Places(Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("b", "e"), Place("e", "c")), Places()),
      "e" -> Edges(Places(Place("a"), Place("a")), Places(Place("d"), Place("d")))
    )
    val trace = Trace("a", "b", "c", "d")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "d")
    individual.getPrecision should be (1/5.0 +- 0.000001)
  }

  test("getPrecision.process_mining_in_a_large_a_tutorial_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b", "e"), Place("e", "c"))),
      "b" -> Edges(Places(Place("a")), Places(Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("b", "e"), Place("e", "c")), Places()),
      "e" -> Edges(Places(Place("a"), Place("a")), Places(Place("d"), Place("d")))
    )
    val trace = Trace("a", "d")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "d")
    individual.getPrecision should be (1/4.0 +- 0.000001)
  }

  test("getPrecision.buch_s_197_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("c", "b"), Place("d"))),
      "b" -> Edges(Places(Place("a")), Places(Place("e"))),
      "c" -> Edges(Places(Place("a")), Places(Place("e"))),
      "d" -> Edges(Places(Place("a")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c", "b"), Place("d")), Places(Place("g", "h", "f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("c", "b"), Place("d"))),
      "g" -> Edges(Places(Place("e")), Places(Place("x"))),
      "h" -> Edges(Places(Place("e")), Places(Place("x"))),
      "x" -> Edges(Places(Place("h", "g")), Places())
    )
    val trace = Trace("a", "c", "d", "e", "h", "x")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "x")
    individual.getPrecision should be (1/9.0 +- 0.000001)
  }

  test("getPrecision.buch_s_197_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("c", "b"), Place("d"))),
      "b" -> Edges(Places(Place("a")), Places(Place("e"))),
      "c" -> Edges(Places(Place("a")), Places(Place("e"))),
      "d" -> Edges(Places(Place("a")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c", "b"), Place("d")), Places(Place("f", "h", "g"))),
      "f" -> Edges(Places(Place("e")), Places(Place("c", "b"), Place("d"))),
      "g" -> Edges(Places(Place("e")), Places(Place("x"))),
      "h" -> Edges(Places(Place("e")), Places(Place("x"))),
      "x" -> Edges(Places(Place("e", "g")), Places())
    )
    val trace = Trace("a", "c", "d", "e")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "x")
    individual.getPrecision should be (1/8.0 +- 0.000001)
  }

  test("getPrecision.buch_s_199_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b", "c"))),
      "b" -> Edges(Places(Place("a")), Places(Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("b", "c")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d")), Places(Place("g", "h", "f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("b", "c"))),
      "g" -> Edges(Places(Place("e")), Places(Place("x"))),
      "h" -> Edges(Places(Place("e")), Places(Place("x"))),
      "x" -> Edges(Places(Place("h", "g")), Places())
    )
    val trace = Trace("a", "d", "c", "e", "h", "x")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "x")
    individual.getPrecision should be (1/9.0 +- 0.000001)
  }

  test("getPrecision.buch_s_199_2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b", "c"))),
      "b" -> Edges(Places(Place("a")), Places(Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("d"))),
      "d" -> Edges(Places(Place("b", "c")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d")), Places(Place("f", "h", "g"))),
      "f" -> Edges(Places(Place("e")), Places(Place("b", "c"))),
      "g" -> Edges(Places(Place("e")), Places(Place("x"))),
      "h" -> Edges(Places(Place("e")), Places(Place("x"))),
      "x" -> Edges(Places(Place("h", "g")), Places())
    )
    val trace = Trace("a", "c", "d", "e", "h", "x")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "x")
    individual.getPrecision should be (1/9.0 +- 0.000001)
  }

  test("getPrecision.buch_s_200") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("a")), Places(Place("e"))),
      "d" -> Edges(Places(Place("a")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c"), Place("d")), Places(Place("h"))),
      "h" -> Edges(Places(Place("e")), Places())
    )
    val trace = Trace("a", "d", "e")

    val individual = new Individual(causalMatrix, EventLog(trace), "a", "h")
    individual.getPrecision should be (1/4.0 +- 0.000001)
  }

  test("getPrecision.incorrect_petri_nets_1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b", "c", "d"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("a")), Places()),
      "d" -> Edges(Places(Place("a")), Places()),
      "e" -> Edges(Places(), Places(Place("f"), Place("h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places())
    )
    val trace = Trace("a", "b", "c", "d", "e", "f", "g")

    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    individual.getPrecision should be (1/7.0 +- 0.000001)
  }

  test("getPrecision.incorrect_petri_nets_2") {
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
    individual.getPrecision should be (1/5.0 +- 0.000001)
  }

  test("getPrecision.incorrect_petri_nets_3") {
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
    individual.getPrecision should be (1/5.0 +- 0.000001)
  }

  test("getPrecision.incorrect_petri_nets_4") {
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
    individual.getPrecision should be (1/5.0 +- 0.000001)
  }

  test("getPrecision.autokauf_8") {
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
    individual.getPrecision should be (1/6.0 +- 0.000001)
  }

  test("getPrecision.autokauf_9") {
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
    individual.getPrecision should be (1/9.0 +- 0.000001)
  }

  test("precision.paper_ProDiGen_fig_4") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("a", "b"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("a", "b")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
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
    val result = individual.getPrecision
    result should be ((1 / 36.0) +- 0.0000001)
  }

  test("precision.paper_ProDiGen_fig_5_a") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
    )
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val individual = new Individual(causalMatrix, eventLog, "x", "y")
    val result = individual.getPrecision
    result should be ((1 / 12.0) +- 0.0000001)
  }

  test("precision.paper_ProDiGen_fig_5_b") {
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
    val result = individual.getPrecision
    result should be ((1 / 14.0) +- 0.0000001)
  }

  test("precision.paper_ProDiGen_fig_5_d") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("a", "b"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c", "d"))),
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
    val result = individual.getPrecision
    result should be ((1 / 6.0) +- 0.0000001)
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
    val result = individual.getPrecision
    result should be (1/42.0 +- 0.000001)
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
    val result = individual.getPrecision

    result should be (1/38.0 +- 0.0000001)
  }
}
