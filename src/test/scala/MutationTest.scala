import de.bachelorarbeit.geneticminer.{EventLog, Individual}
import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable


class MutationTest extends FunSuite {
  test("autokauf.f") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e"), Place("x")), Places(Place("g"), Place("u"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )

    val trace = Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")
    individual.randomObject.setSeed(800000) //so we are mutating task f
    individual.mutate()

    val input = individual.getInput("f")
    input should be (Places(Place("x")))
    individual.getOutput("e") should be (Places(Place("h")))

    val output = individual.getOutput("f")
    output should be (Places(Place("u", "g"), Place("g")))
    individual.getInput("g") should be (Places(Place("f"), Place("f")))
  }

  test("autokauf.h") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e"), Place("u")), Places(Place("c"), Place("d")))
    )

    val individual = new Individual(causalMatrix, EventLog(Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g")), "a", "g")
    individual.randomObject.setSeed(3L)
    individual.mutate()

    val input = individual.getInput("h")
    input should be (Places(Place("e"), Place("u"), Place("c")))
    individual.getOutput("e") should be (Places(Place("f"), Place("h")))

    val output = individual.getOutput("h")
    output should be (Places(Place("c")))
    individual.getInput("e") should be (Places(Place("d"), Place("c")))
  }

  test("autokauf.g") {

    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"))

    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(), Places(Place("c"), Place("d")))
    )

    val individual = new Individual(causalMatrix, eventLog, "a", "g")
    individual.randomObject.setSeed(10000000)

    individual.mutate()
    val input = individual.getInput("e")

    input should be (Places(Place("d"), Place("c"), Place("h")))
    individual.getOutput("h") should be (Places(Place("c"), Place("d"), Place("e")))

    val output = individual.getOutput("e")
    output should be (Places(Place("f")))
    individual.getInput("f") should be (Places(Place("e")))
    individual.getInput("h") should be (Places())
  }
}
