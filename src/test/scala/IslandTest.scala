import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace}
import de.bachelorarbeit.geneticminer.{Individual, Island, EventLog}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable

class IslandTest extends FunSuite {
  test("copyIndividual") {
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

    val eventLog = EventLog(Trace("a", "b", "c", "d", "e", "f", "g"))
    val individual = new Individual(causalMatrix, eventLog, "a", "g")
    val copy = individual.copy()

    individual.startTask should be (copy.startTask)
    individual.endTask should be (copy.endTask)
    individual.eventLog should be (copy.eventLog)
    individual.causalMatrix should be (copy.causalMatrix)
    individual.getPrecision should be (copy.getPrecision +- 0.000001)
    individual.getSimplicity should be (copy.getSimplicity +- 0.000001)
    individual.getCompleteness should be (copy.getCompleteness +- 0.000001)

    copy.causalMatrix("d").inputs.remove(0)
    copy.causalMatrix("b").outputs.remove(1)
    copy.causalMatrix should not be individual.causalMatrix
    copy.calculateFitnessValues()
    individual.getPrecision should not be (copy.getPrecision +- 0.000001)
    individual.getSimplicity should not be (copy.getSimplicity +- 0.000001)
    individual.getCompleteness should not be (copy.getCompleteness +- 0.000001)

    copy.causalMatrix("d").inputs.append(Place("b", "h"))
    copy.causalMatrix("b").outputs.append(Place("d"))
    copy.calculateFitnessValues()
    individual.getPrecision should be (copy.getPrecision +- 0.000001)
    individual.getSimplicity should be (copy.getSimplicity +- 0.000001)
    individual.getCompleteness should be (copy.getCompleteness +- 0.000001)
    copy.causalMatrix should be (individual.causalMatrix)

    copy.causalMatrix("h").outputs.append(Place("y"))
    copy.calculateFitnessValues()
    copy.causalMatrix should not be individual.causalMatrix
    individual.getPrecision should be (copy.getPrecision +- 0.000001)
    individual.getSimplicity should not be (copy.getSimplicity +- 0.000001)
    individual.getCompleteness should be (copy.getCompleteness +- 0.000001)
  }

  test("tournamentSelection.1") {
    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"),
      Trace("a", "i", "a", "i", "a", "b", "d", "c", "e", "f", "g"))

    val proDiGen = new Island(eventLog, POPULATION_SIZE = 10, CROSSOVER_RATE = 0.3, P = 5)
    proDiGen.randomObject.setSeed(2000000)

    val individuals = proDiGen.createInitialPopulationSingleThreaded()
    val selection = proDiGen.tournamentSelection(individuals)

    val maps = Vector(mutable.Map("e" -> Edges(Places(Place("d")),Places(Place("f"), Place("h"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("i", "b"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(Place("a")),Places(Place("c"), Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("b")),Places(Place("d"))), "h" -> Edges(Places(Place("e")),Places()), "d" -> Edges(Places(Place("c", "b")),Places(Place("e")))),
    mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(),Places(Place("i"), Place("b"))), "i" -> Edges(Places(Place("a")),Places()), "b" -> Edges(Places(Place("a")),Places(Place("c"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("b")),Places(Place("e"))), "h" -> Edges(Places(),Places()), "d" -> Edges(Places(),Places(Place("e")))),
    mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("b"), Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(Place("a")),Places(Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("h")),Places(Place("d"), Place("e"))), "h" -> Edges(Places(),Places(Place("c"), Place("d"))), "d" -> Edges(Places(Place("c", "b"), Place("h")),Places(Place("e")))),
    mutable.Map("e" -> Edges(Places(Place("c")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(),Places(Place("b"))), "i" -> Edges(Places(),Places()), "b" -> Edges(Places(Place("a")),Places(Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(),Places(Place("e"))), "h" -> Edges(Places(),Places(Place("d"))), "d" -> Edges(Places(Place("b"), Place("h")),Places())),
    mutable.Map("e" -> Edges(Places(Place("d")),Places(Place("h"), Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(),Places()), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("h")),Places()), "h" -> Edges(Places(Place("e")),Places(Place("c"))), "d" -> Edges(Places(),Places(Place("e")))),
    mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("b"), Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(Place("a")),Places(Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("h")),Places(Place("d"), Place("e"))), "h" -> Edges(Places(),Places(Place("c"), Place("d"))), "d" -> Edges(Places(Place("c", "b"), Place("h")),Places(Place("e")))),
    mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("b"), Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(Place("a")),Places(Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("h")),Places(Place("d"), Place("e"))), "h" -> Edges(Places(),Places(Place("c"), Place("d"))), "d" -> Edges(Places(Place("c", "b"), Place("h")),Places(Place("e")))),
    mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(Place("i")),Places(Place("i"))), "i" -> Edges(Places(Place("a")),Places(Place("a"))), "b" -> Edges(Places(),Places(Place("c"), Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("h"), Place("b")),Places(Place("e"))), "h" -> Edges(Places(),Places(Place("c"), Place("d"))), "d" -> Edges(Places(Place("h"), Place("b")),Places(Place("e")))),
    mutable.Map("e" -> Edges(Places(Place("c"), Place("d")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(),Places(Place("i"), Place("b"))), "i" -> Edges(Places(Place("a")),Places()), "b" -> Edges(Places(Place("a")),Places(Place("c"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(Place("b")),Places(Place("e"))), "h" -> Edges(Places(),Places()), "d" -> Edges(Places(),Places(Place("e")))),
    mutable.Map("e" -> Edges(Places(Place("c")),Places(Place("f"))), "f" -> Edges(Places(Place("e")),Places(Place("g"))), "a" -> Edges(Places(),Places(Place("b"))), "i" -> Edges(Places(),Places()), "b" -> Edges(Places(Place("a")),Places(Place("d"))), "g" -> Edges(Places(Place("f")),Places()), "c" -> Edges(Places(),Places(Place("e"))), "h" -> Edges(Places(),Places(Place("d"))), "d" -> Edges(Places(Place("b"), Place("h")),Places())))

    selection.zipWithIndex.foreach(pair => {
      val individual = pair._1
      val index = pair._2
      val map = maps(index)
      individual.causalMatrix should be (map)
    })
  }

  test("tournamentSelection.2") {

    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"))

    val causalMatrixA = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )
    //Completeness=1,000, Precision=0,024, Simplicity=0,050
    val individualA = new Individual(causalMatrixA, eventLog, "a", "g")

    val causalMatrixB = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places())
    )
    //Completeness=0,861, Precision=0,026, Simplicity=0,056
    val individualB = new Individual(causalMatrixB, eventLog, "a", "g")

    val causalMatrixC = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"), Place("p"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "p" -> Edges(Places(Place("e")), Places())
    )
    //Completeness=0,833, Precision=0,021, Simplicity=0,045
    val individualC = new Individual(causalMatrixC, eventLog, "a", "g")

    val causalMatrixD = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "p" -> Edges(Places(Place("p"), Place("z")), Places()),
      "z" -> Edges(Places(Place("p")), Places())
    )
    //Completeness=1,000, Precision=0,024, Simplicity=0,043
    val individualD = new Individual(causalMatrixD, eventLog, "a", "g")

    val proDiGen = new Island(eventLog, POPULATION_SIZE = 4, CROSSOVER_RATE = 0.3, P = 5)
    proDiGen.randomObject.setSeed(20000000)
    val population = IndexedSeq(individualA, individualB, individualC, individualD)

    val selection = proDiGen.tournamentSelection(population)
    val expectedSelection = IndexedSeq(individualA, individualA, individualC, individualA)

    selection should be (expectedSelection)
  }

  test("replacement") {
    val eventLog = EventLog(Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"))

    val causalMatrixA = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )
    //Completeness=1,000, Precision=0,024, Simplicity=0,050
    val individualA = new Individual(causalMatrixA, eventLog, "a", "g")

    val causalMatrixB = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places())
    )
    //Completeness=0,861, Precision=0,026, Simplicity=0,056
    val individualB = new Individual(causalMatrixB, eventLog, "a", "g")

    val causalMatrixC = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"), Place("p"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "p" -> Edges(Places(Place("e")), Places())
    )
    //Completeness=0,833, Precision=0,021, Simplicity=0,045
    val individualC = new Individual(causalMatrixC, eventLog, "a", "g")

    val causalMatrixD = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d"))),
      "p" -> Edges(Places(Place("p"), Place("z")), Places()),
      "z" -> Edges(Places(Place("p")), Places())
    )
    //Completeness=1,000, Precision=0,024, Simplicity=0,043
    val individualD = new Individual(causalMatrixD, eventLog, "a", "g")

    val currentPopulation = mutable.ArrayBuffer(individualA, individualB, individualD, individualB)
    val offsprings = mutable.ArrayBuffer(individualA, individualC, individualD, individualA)

    val proDiGen = new Island(eventLog, POPULATION_SIZE = 4, CROSSOVER_RATE = 0.3, P = 5)

    val replaced = proDiGen.replacement(currentPopulation, offsprings)
    val expected = IndexedSeq(individualA, individualD, individualB, individualC)

    replaced should be (expected)
  }

  test("noNewIndividuals.1") {
    val eventLog = EventLog()

    val maps1 = List(mutable.Map("e" -> Edges(Places(Place("c")),Places()), "x" -> Edges(Places(),Places(Place("b"))), "y" -> Edges(Places(Place("d")),Places()), "a" -> Edges(Places(),Places()), "b" -> Edges(Places(Place("x")),Places()), "c" -> Edges(Places(),Places(Place("e"))), "d" -> Edges(Places(),Places(Place("y")))),
      mutable.Map("e" -> Edges(Places(Place("c")),Places()), "x" -> Edges(Places(),Places()), "y" -> Edges(Places(Place("d")),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(),Places(Place("c"))), "c" -> Edges(Places(Place("a", "b")),Places(Place("e"))), "d" -> Edges(Places(),Places(Place("y")))),
      mutable.Map("e" -> Edges(Places(),Places(Place("y"))), "x" -> Edges(Places(),Places()), "y" -> Edges(Places(Place("e")),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(),Places()), "c" -> Edges(Places(Place("a")),Places(Place("d"))), "d" -> Edges(Places(Place("c")),Places())),
      mutable.Map("e" -> Edges(Places(),Places()), "x" -> Edges(Places(),Places(Place("b"))), "y" -> Edges(Places(),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(Place("x")),Places(Place("c"))), "c" -> Edges(Places(Place("b"), Place("a")),Places()), "d" -> Edges(Places(),Places())),
      mutable.Map("e" -> Edges(Places(Place("c")),Places(Place("y"))), "x" -> Edges(Places(),Places(Place("a"), Place("b"))), "y" -> Edges(Places(Place("d"), Place("e")),Places()), "a" -> Edges(Places(Place("x")),Places(Place("c"))), "b" -> Edges(Places(Place("x")),Places(Place("c"))), "c" -> Edges(Places(Place("a", "b")),Places(Place("d"), Place("e"))), "d" -> Edges(Places(Place("c")),Places(Place("y"))))
    )

    val indis1 = maps1.map(m => new Individual(m, eventLog, "x", "y")).toIndexedSeq

    val maps2 = List(mutable.Map("e" -> Edges(Places(Place("c")),Places()), "x" -> Edges(Places(),Places(Place("b"))), "y" -> Edges(Places(Place("d")),Places()), "a" -> Edges(Places(),Places()), "b" -> Edges(Places(Place("x")),Places()), "c" -> Edges(Places(),Places(Place("e"))), "d" -> Edges(Places(),Places(Place("y")))),
      mutable.Map("e" -> Edges(Places(Place("c")),Places()), "x" -> Edges(Places(),Places()), "y" -> Edges(Places(Place("d")),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(),Places(Place("c"))), "c" -> Edges(Places(Place("a", "b")),Places(Place("e"))), "d" -> Edges(Places(),Places(Place("y")))),
      mutable.Map("e" -> Edges(Places(),Places(Place("y"))), "x" -> Edges(Places(),Places()), "y" -> Edges(Places(Place("e")),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(),Places()), "c" -> Edges(Places(Place("a")),Places(Place("d"))), "d" -> Edges(Places(Place("c")),Places())),
      mutable.Map("e" -> Edges(Places(),Places()), "x" -> Edges(Places(),Places(Place("b"))), "y" -> Edges(Places(),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(Place("x")),Places(Place("c"))), "c" -> Edges(Places(Place("b"), Place("a")),Places()), "d" -> Edges(Places(),Places())),
      mutable.Map("e" -> Edges(Places(Place("c")),Places(Place("y"))), "x" -> Edges(Places(),Places(Place("a"), Place("b"))), "y" -> Edges(Places(Place("d"), Place("e")),Places()), "a" -> Edges(Places(Place("x")),Places(Place("c"))), "b" -> Edges(Places(Place("x")),Places(Place("c"))), "c" -> Edges(Places(Place("a", "b")),Places(Place("d"), Place("e"))), "d" -> Edges(Places(Place("c")),Places(Place("y"))))
    )

    val indis2 = maps2.map(m => new Individual(m, eventLog, "x", "y")).toIndexedSeq

    Island.noNewIndividuals(indis1, indis2) should be (true)
    Island.noNewIndividuals(indis2, indis1) should be (true)


    val maps3 = List(mutable.Map("e" -> Edges(Places(Place("c")),Places()), "x" -> Edges(Places(),Places(Place("b"))), "y" -> Edges(Places(Place("d")),Places()), "a" -> Edges(Places(),Places()), "b" -> Edges(Places(Place("x")),Places()), "c" -> Edges(Places(),Places(Place("e"))), "d" -> Edges(Places(),Places(Place("y")))),
      mutable.Map("e" -> Edges(Places(Place("c")),Places()), "x" -> Edges(Places(),Places()), "y" -> Edges(Places(Place("d")),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(),Places(Place("c"))), "c" -> Edges(Places(Place("a", "b")),Places(Place("e"))), "d" -> Edges(Places(),Places(Place("y")))),
      mutable.Map("e" -> Edges(Places(),Places(Place("y"))), "x" -> Edges(Places(),Places()), "y" -> Edges(Places(Place("e")),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(),Places()), "c" -> Edges(Places(Place("a")),Places(Place("d"))), "d" -> Edges(Places(Place("c")),Places())),
      mutable.Map("e" -> Edges(Places(),Places()), "x" -> Edges(Places(),Places(Place("by"))), "y" -> Edges(Places(Place("x")),Places()), "a" -> Edges(Places(),Places(Place("c"))), "b" -> Edges(Places(Place("x")),Places(Place("c"))), "c" -> Edges(Places(Place("b"), Place("a")),Places()), "d" -> Edges(Places(),Places())),
      mutable.Map("e" -> Edges(Places(Place("c")),Places(Place("y"))), "x" -> Edges(Places(),Places(Place("a"), Place("b"))), "y" -> Edges(Places(Place("d"), Place("e")),Places()), "a" -> Edges(Places(Place("x")),Places(Place("c"))), "b" -> Edges(Places(Place("x")),Places(Place("c"))), "c" -> Edges(Places(Place("a", "b")),Places(Place("d"), Place("e"))), "d" -> Edges(Places(Place("c")),Places(Place("y"))))
    ) //Changes map 4

    val indis3 = maps3.map(m => new Individual(m, eventLog, "x", "y")).toIndexedSeq

    Island.noNewIndividuals(indis1, indis3) should be (false)
    Island.noNewIndividuals(indis3, indis1) should be (false)
  }

  test("areIndividualsEqual") {
    val causalMatrixA = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )

    val individualA = new Individual(causalMatrixA, EventLog(), "x", "y")

    val causalMatrixB = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )

    val individualB = new Individual(causalMatrixB, EventLog(), "x", "y")

    Island.areIndividualsEqual(individualA, individualB) should be (true)
    Island.areIndividualsEqual(individualB, individualA) should be (true)

    val causalMatrixC = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )

    val individualC = new Individual(causalMatrixC, EventLog(), "x", "y")

    Island.areIndividualsEqual(individualA, individualC) should be (false)
    Island.areIndividualsEqual(individualC, individualA) should be (false)
  }


}
