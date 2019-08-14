import de.bachelorarbeit.geneticminer.{EventLog, Individual, Island}
import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace, Task}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable


class CrossoverTest extends FunSuite {

  test("crossover.incorrectlyFiredActivitiesSet.1") {
    val eventLog = EventLog(
      Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g")
    )

    val causalMatrixA = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("x"))),
      "c" -> Edges(Places(Place("h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"), Place("z"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("l")), Places(Place("c"), Place("d"))),
      "p" -> Edges(Places(), Places()),
      "z" -> Edges(Places(Place("f")), Places()),
      "x" -> Edges(Places(Place("b")), Places()),
      "l" -> Edges(Places(), Places(Place("h"))),
      "u" -> Edges(Places(), Places()),
      "w" -> Edges(Places(), Places())
    )

    val individualA = new Individual(causalMatrixA, eventLog, "a", "g")

    val causalMatrixB = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"), Place("x"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"), Place("z"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("p"), Place("e")), Places(Place("c"), Place("d", "u", "w"))),
      "p" -> Edges(Places(), Places(Place("h"))),
      "z" -> Edges(Places(Place("f")), Places()),
      "x" -> Edges(Places(Place("b")), Places()),
      "l" -> Edges(Places(), Places()),
      "u" -> Edges(Places(Place("h")), Places()),
      "w" -> Edges(Places(Place("h")), Places())
    )
    val individualB = new Individual(causalMatrixB, eventLog, "a", "g")

    val proDiGen = new Island(eventLog, POPULATION_SIZE = 4, CROSSOVER_RATE = 1, P = 5)
    proDiGen.randomObject.setSeed(10000)

    val incorrectTasks = proDiGen.getIncorrectlyFiredActivitiesSet(individualA, individualB)
    incorrectTasks.size should be (3)
    incorrectTasks.toSet should be (Set("b", "f", "h"))
  }

  test("crossover.incorrectlyFiredActivitiesSet.2") {
    val eventLog = EventLog(Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"))

    val causalMatrixA = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )

    val individualA = new Individual(causalMatrixA, eventLog, "x", "y")

    val causalMatrixB = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )
    val individualB = new Individual(causalMatrixB, eventLog, "x", "y")

    val proDiGen = new Island(eventLog, POPULATION_SIZE = 4, CROSSOVER_RATE = 1, P = 5)
    proDiGen.randomObject.setSeed(10000)

    val incorrectTasks = proDiGen.getIncorrectlyFiredActivitiesSet(individualA, individualB)
    incorrectTasks should be (IndexedSeq.empty[Task])
  }

  test("crossover.incorrectlyFiredActivitiesSet.3") {
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"))

    val causalMatrixA = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places())
    )

    val individualA = new Individual(causalMatrixA, eventLog, "x", "y")

    val causalMatrixB = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b", "a"), Place("l")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d", "e")), Places()),
      "l" -> Edges(Places(), Places(Place("c")))
    )
    val individualB = new Individual(causalMatrixB, eventLog, "x", "y")

    val proDiGen = new Island(eventLog, POPULATION_SIZE = 4, CROSSOVER_RATE = 1, P = 5)
    proDiGen.randomObject.setSeed(10000)

    individualA.setInput("u", Places(Place("e")))
    individualA.setOutput("e", Places(Place("y"), Place("u")))
    individualA.calculateFitnessValues()

    var incorrectTasks = proDiGen.getIncorrectlyFiredActivitiesSet(individualA, individualB)
    incorrectTasks should be (IndexedSeq("e"))

    individualA.setInput("n", Places(Place("d")))
    individualA.setOutput("d", Places(Place("y"), Place("n")))
    individualA.calculateFitnessValues()

    incorrectTasks = proDiGen.getIncorrectlyFiredActivitiesSet(individualA, individualB)
    incorrectTasks.size should be (2)
    incorrectTasks.toSet should be (Set("e", "d"))
  }

  test("crossover.1") {
    val eventLog = EventLog(
      Trace("a", "b", "d", "c", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "c", "d", "e", "f", "g"),
      Trace("a", "b", "c", "d", "e", "h", "d", "c", "e", "f", "g"))

    val causalMatrixA = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("x"))),
      "c" -> Edges(Places(Place("h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"), Place("z"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("l")), Places(Place("c"), Place("d"))),
      "p" -> Edges(Places(), Places()),
      "z" -> Edges(Places(Place("f")), Places()),
      "x" -> Edges(Places(Place("b")), Places()),
      "l" -> Edges(Places(), Places(Place("h"))),
      "u" -> Edges(Places(), Places()),
      "w" -> Edges(Places(), Places())
    )

    val individualA = new Individual(causalMatrixA, eventLog, "a", "g")

    val causalMatrixB = mutable.Map(
      "a" -> Edges(Places(), Places(Place("b"))),
      "b" -> Edges(Places(Place("a")), Places(Place("c"), Place("d"), Place("x"))),
      "c" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "d" -> Edges(Places(Place("b", "h")), Places(Place("e"))),
      "e" -> Edges(Places(Place("d"), Place("c")), Places(Place("f", "h"))),
      "f" -> Edges(Places(Place("e")), Places(Place("g"), Place("z"))),
      "g" -> Edges(Places(Place("f")), Places()),
      "h" -> Edges(Places(Place("p"), Place("e")), Places(Place("c"), Place("d", "u", "w"))),
      "p" -> Edges(Places(), Places(Place("h"))),
      "z" -> Edges(Places(Place("f")), Places()),
      "x" -> Edges(Places(Place("b")), Places()),
      "l" -> Edges(Places(), Places()),
      "u" -> Edges(Places(Place("h")), Places()),
      "w" -> Edges(Places(Place("h")), Places())
    )
    val individualB = new Individual(causalMatrixB, eventLog, "a", "g")

    val proDiGen = new Island(eventLog, POPULATION_SIZE = 4, CROSSOVER_RATE = 1, P = 5)
    proDiGen.randomObject.setSeed(10000)
    //the next two lines are only to get the desired crossover results. Try to remove that!
    proDiGen.randomObject.nextDouble()
    proDiGen.randomObject.nextInt(3)

    proDiGen.crossoverTask("h", individualA, individualB)

    individualA.getInput("h") should be (Places(Place("e")))
    individualA.getOutput("h") should be (Places())
    individualB.getInput("h") should be (Places(Place("l")))
    individualB.getOutput("h") should be (Places(Place("c"), Place("d")))
  }
}
