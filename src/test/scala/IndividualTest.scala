
import de.bachelorarbeit.geneticminer.{EventLog, Individual}
import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable

class IndividualTest extends FunSuite {

  test("isEqual") {
    Individual.isEqual(1, 1) should be (true)
    Individual.isEqual(1.000000001, 1.000000008) should be (true)
    Individual.isEqual(1.010000001, 1.020000000) should not be true
    Individual.isEqual(1.010000001, 1.020000001) should not be true
    Individual.isEqual(1.010001001, 1.010001001) should be (true)
    Individual.isEqual(1.010000200, 1.010000100) should be (true)
  }

  test("individualsEqualsAndHashCode") {
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

    assert(individualA == individualB)
    individualA should be (individualB)
    individualA.hashCode() should be (individualB.hashCode())

    individualA.setOutput("e", Places(Place("a")))

    assert(individualA != individualB)
    individualB should not be individualA
    individualA.hashCode() should not be individualB.hashCode()

    individualA.setOutput("e", Places(Place("y")))
    assert(individualA == individualB)
    individualA should be (individualB)
    individualA.hashCode() should be (individualB.hashCode())

    val individualC = new Individual(causalMatrixA, EventLog(), "r", "y")
    individualC should not be individualA

    val individualD = new Individual(causalMatrixA, EventLog(), "x", "a")
    individualD should not be individualA
  }

  test("repairOutput.1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a", "h")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )

    val trace = Trace("a", "b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")

    val newInputOfE = Places(Place("c"), Place("c", "a"))
    val oldInputTasksOfE = Individual.convertPlacesToListOfTask(individual.getInput("e"))
    individual.setInput("e", newInputOfE)
    individual.repairOutput("e", oldInputTasksOfE)

    individual.getOutput("d").size should be (0)
    individual.getOutput("a").count(place => place.contains("e")) should be (1)
    individual.getOutput("a").count(place => place.contains("b")) should be (1)
    individual.getOutput("c").count(place => place.contains("e")) should be (2)
    individual.getInput("e") should be (Places(Place("c"), Place("c", "a")))
  }

  test("repairInput.1") {
    val causalMatrix = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a", "h")), Places(Place("c"), Place("d"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("e")), Places(Place("c"), Place("d")))
    )

    val trace = Trace("a", "b", "c", "d", "e", "f", "g")
    val individual = new Individual(causalMatrix, EventLog(trace), "a", "g")

    val newOutputOfE = Places(Place("b", "c"), Place("h"), Place("b"))
    val oldOutputTasksOfE = Individual.convertPlacesToListOfTask(individual.getOutput("e"))

    individual.setOutput("e", newOutputOfE)
    individual.repairInput("e", oldOutputTasksOfE)

    individual.getInput("f").size should be (0)
    individual.getInput("b").count(place => place.contains("e")) should be (2)
    individual.getInput("b").count(place => place.contains("a")) should be (1)
    individual.getInput("c").count(place => place.contains("e")) should be (1)
    individual.getInput("h").count(place => place.contains("e")) should be (1)
    individual.getOutput("e") should be (Places(Place("b", "c"), Place("h"), Place("b")))
  }

  test("input_outputDependencies.1") {
    val eventLog = EventLog(Trace("a", "b", "c", "d", "e", "f", "g"), Trace("a", "x", "y", "a"))

    val outputDependencies = mutable.Map(
      "a" -> IndexedSeq("b", "c", "d", "e", "f", "g", "x", "y"),
      "b" -> IndexedSeq("c", "d", "e", "f", "g"),
      "c" -> IndexedSeq("d", "e", "f", "g"),
      "d" -> IndexedSeq("e", "f", "g"),
      "e" -> IndexedSeq("f", "g"),
      "f" -> IndexedSeq("g"),
      "g" -> IndexedSeq(),
      "x" -> IndexedSeq("y", "a"),
      "y" -> IndexedSeq("a")
      //"h" -> IndexedSeq()
    )

    val inputDependencies = mutable.Map(
      "a" -> IndexedSeq("x", "y"),
      "b" -> IndexedSeq("a"),
      "c" -> IndexedSeq("a", "b"),
      "d" -> IndexedSeq("a", "b", "c"),
      "e" -> IndexedSeq("a", "b", "c", "d"),
      "f" -> IndexedSeq("a", "b", "c", "d", "e"),
      "g" -> IndexedSeq("a", "b", "c", "d", "e", "f"),
      "x" -> IndexedSeq("a"),
      "y" -> IndexedSeq("a", "x")
      //"h" -> IndexedSeq()
    )

    eventLog.outputDependenciesForMutation should be (outputDependencies)
    eventLog.inputDependenciesForMutation should be (inputDependencies)
  }


}
