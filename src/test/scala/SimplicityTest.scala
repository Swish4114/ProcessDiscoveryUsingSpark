import de.bachelorarbeit.geneticminer.{EventLog, Individual}
import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable

class SimplicityTest extends FunSuite {
  test("simplicity.ProDiGen_fig_4") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("a", "b"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("a", "b")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )
    val individual = new Individual(causalMatrix, EventLog(), "x", "y")
    val result = individual.getSimplicity
    result should be ((1 / 20.0) +- 0.0000001)
  }

  test("simplicity.ProDiGen_fig_b") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("a", "b"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("a", "b")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )
    val individual = new Individual(causalMatrix, EventLog(), "x", "y")
    val result = individual.getSimplicity
    result should be ((1 / 16.0) +- 0.0000001)
  }

  test("simplicity.ProDiGen_fig_c") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b")), Places(Place("e"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e")), Places())
    )
    val individual = new Individual(causalMatrix, EventLog(), "x", "y")
    val result = individual.getSimplicity
    result should be ((1 / 8.0) +- 0.0000001)
  }

  test("simplicity.ProDiGen_fig_d") {
    val causalMatrix = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c", "d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c", "e"))),
      "c" -> Edges(Places(Place("a"), Place("b")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d"), Place("e")), Places())
    )
    val individual = new Individual(causalMatrix, EventLog(), "x", "y")
    val result = individual.getSimplicity
    result should be ((1 / 20.0) +- 0.0000001)
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

    val individual = new Individual(causalMatrix, EventLog(), "a", "g")
    val result = individual.getSimplicity

    result should be ((1/20.0) +- 0.00000001)
  }

  test("autokauf.2") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places())
    )

    val individual = new Individual(causalMatrix, EventLog(), "a", "g")
    val result = individual.getSimplicity

    result should be ((1/17.0) +- 0.00000001)
  }
}
