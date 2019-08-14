import de.bachelorarbeit.geneticminer.{EventLog, Individual, Island}
import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable

class InitialPopulationTest extends FunSuite {
  test("dependencyMeasure.1") {
    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"),
      Trace("a", "i", "a", "i", "a", "b", "d", "c", "e", "f", "g"))

    eventLog.getDependencyMeasure("a", "b") should be (0.833 +- 0.001)
    eventLog.getDependencyMeasure("b", "a") should be (-0.833 +- 0.001)
    eventLog.getDependencyMeasure("a", "i") should be (0.75 +- 0.001)
    eventLog.getDependencyMeasure("h", "e") should be (-0.666 +- 0.001)
    eventLog.getDependencyMeasure("e", "h") should be (0.666 +- 0.001)
    eventLog.getDependencyMeasure("i", "a") should be (0.75 +- 0.001)
    eventLog.getDependencyMeasure("c", "d") should be (0.125 +- 0.001)
    eventLog.getDependencyMeasure("d", "c") should be (-0.125 +- 0.001)
    eventLog.getDependencyMeasure("g", "d") should be (0)
  }

  test("dependencyMeasure.2") {
    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "x", "x", "x", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"),
      Trace("a", "i", "a", "i", "a", "b", "d", "c", "x", "x", "e", "f", "g"))

    eventLog.getDependencyMeasure("x", "x") should be ((3/4.0) +- 0.000001)

  }

  test("createInitialPopulation") {
    val PS = 10
    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"),
      Trace("a", "i", "a", "i", "a", "b", "d", "c", "e", "f", "g"))

    val proDiGen = new Island(eventLog, POPULATION_SIZE = PS, CROSSOVER_RATE = 0.3, P = 5)
    proDiGen.randomObject.setSeed(2000000)

    val maps = IndexedSeq(mutable.Map("e" -> Edges(Places(Place("c")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(),Places(Place("b"))), "i" -> Edges(Places(),Places()), "b" -> Edges(Places(Place("a")),Places(Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(),Places(Place("e"))), "h" -> Edges(Places(),Places(Place("d"))), "d" -> Edges(Places(Place("b"), Place("h")),Places())),
      mutable.Map("e" -> Edges(Places(Place("d")),Places()), "f" -> Edges(Places(),Places(Place("g"))), "a" -> Edges(Places(),Places()), "i" -> Edges(Places(),Places()), "b" -> Edges(Places(),Places(Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(),Places()), "h" -> Edges(Places(),Places()), "d" -> Edges(Places(Place("b")),Places(Place("e")))),
      mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"), Place("h"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(),Places(Place("b"), Place("i"))), "i" -> Edges(Places(Place("a")),Places()), "b" -> Edges(Places(Place("a")),Places(Place("d"), Place("c"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("b")),Places(Place("e"))), "h" -> Edges(Places(Place("e")),Places(Place("d"))), "d" -> Edges(Places(Place("h"), Place("b")),Places(Place("e")))),
      mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(),Places(Place("i"), Place("b"))), "i" -> Edges(Places(Place("a")),Places()), "b" -> Edges(Places(Place("a")),Places(Place("c"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("b")),Places(Place("e"))), "h" -> Edges(Places(),Places()), "d" -> Edges(Places(),Places(Place("e")))),
      mutable.Map("e" -> Edges(Places(Place("d")),Places(Place("f"), Place("h"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("i", "b"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(Place("a")),Places(Place("c"), Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("b")),Places(Place("d"))), "h" -> Edges(Places(Place("e")),Places()), "d" -> Edges(Places(Place("c", "b")),Places(Place("e")))),
      mutable.Map("e" -> Edges(Places(Place("d")),Places(Place("h"), Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(),Places()), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("h")),Places()), "h" -> Edges(Places(Place("e")),Places(Place("c"))), "d" -> Edges(Places(),Places(Place("e")))),
      mutable.Map("e" -> Edges(Places(),Places()), "f" -> Edges(Places(),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places()), "i" -> Edges(Places(),Places(Place("a"))), "b" -> Edges(Places(),Places(Place("c"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("b")),Places()), "h" -> Edges(Places(),Places()), "d" -> Edges(Places(),Places())),
      mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(),Places(Place("c"), Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("h"), Place("b")),Places(Place("e"))), "h" -> Edges(Places(),Places(Place("c"), Place("d"))), "d" -> Edges(Places(Place("h"), Place("b")),Places(Place("e")))),
      mutable.Map("e" -> Edges(Places(Place("d"), Place("c")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(),Places(Place("c"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("b"), Place("h")),Places(Place("e"))), "h" -> Edges(Places(),Places(Place("c"), Place("d"))), "d" -> Edges(Places(Place("h")),Places(Place("e")))),
      mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("b"), Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(Place("a")),Places(Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("h")),Places(Place("d"), Place("e"))), "h" -> Edges(Places(),Places(Place("c"), Place("d"))), "d" -> Edges(Places(Place("c", "b"), Place("h")),Places(Place("e"))))
    )

    val individuals = (0 until PS).map(_ => proDiGen.createInitialIndividual())

    individuals.zipWithIndex.foreach(pair => {
      val individual = pair._1
      val index = pair._2
      val map = maps(index)
      individual.causalMatrix should be (map)
    })

    var population = proDiGen.createInitialPopulation(individuals: _*)
    population.size should be  (PS)

    population.zipWithIndex.foreach(pair => {
      val individual = pair._1
      val index = pair._2
      val map = maps(index)
      individual.causalMatrix should be (map)
    })

    population = proDiGen.createInitialPopulation()
    population.size should be (PS)
  }
}
