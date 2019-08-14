import de.bachelorarbeit.geneticminer.{EventLog, Individual}
import de.bachelorarbeit.geneticminer.{Edges, Place, Places, Trace}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable

class HasBetterFitnessTest extends FunSuite {
  test("hasBetterFitness.ProDiGen_Fig5_ab_precision") {
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val causalMatrixA = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
    )
    val individualA = new Individual(causalMatrixA, eventLog, "x", "y")

    val causalMatrixB = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
    )

    val individualB = new Individual(causalMatrixB, eventLog, "x", "y")

    individualA.hasBetterFitness(individualB) should be (true)
    individualB.hasBetterFitness(individualA) should be (false)
    individualA.hasBetterFitness(individualA) should be (false)
  }


  test("hasBetterFitness.ProDiGen_Fig5_aa_simplicity") {
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val causalMatrixA = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places()),
      "k" -> Edges(Places(Place("p")), Places())
    )
    val individualA = new Individual(causalMatrixA, eventLog, "x", "y")

    val causalMatrixB = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
    )

    val individualB = new Individual(causalMatrixB, eventLog, "x", "y")

    individualA.hasBetterFitness(individualB) should be (false)
    individualB.hasBetterFitness(individualA) should be (true)
    individualA.hasBetterFitness(individualA) should be (false)
  }

  test("hasBetterFitness.ProDiGen_Fig5_ad_completeness") {

    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y")
    )

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
      "x" -> Edges(Places(), Places(Place("a", "b"))),
      "a" -> Edges(Places(Place("x")), Places(Place("d", "c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c", "e"))),
      "c" -> Edges(Places(Place("a"), Place("b")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d"), Place("e")), Places())
    )
    val individualB = new Individual(causalMatrixB, eventLog, "x", "y")

    individualA.hasBetterFitness(individualB) should be (true)
    individualB.hasBetterFitness(individualA) should be (false)
    individualA.hasBetterFitness(individualA) should be (false)
  }

  test("autokauf.completeness") {
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
    val individualB = new Individual(causalMatrixB, eventLog, "a", "g")

    //println(individualA)
    //println(individualB)

    individualA.hasBetterFitness(individualB) should be (true)
    individualB.hasBetterFitness(individualA) should be (false)
    individualA.hasBetterFitness(individualA) should be (false)
  }


  test("autokauf.precision") {
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
    val individualA = new Individual(causalMatrixA, eventLog, "a", "g")

    val causalMatrixB = mutable.Map(
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
    val individualB = new Individual(causalMatrixB, eventLog, "a", "g")

    //println(individualA)
    //println(individualB)

    individualA.hasBetterFitness(individualB) should be (true)
    individualB.hasBetterFitness(individualA) should be (false)
    individualA.hasBetterFitness(individualA) should be (false)
  }

  test("autokauf.simplicity") {
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
    val individualA = new Individual(causalMatrixA, eventLog, "a", "g")

    val causalMatrixB = mutable.Map(
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
    val individualB = new Individual(causalMatrixB, eventLog, "a", "g")

    individualA.hasBetterFitness(individualB) should be (true)
    individualB.hasBetterFitness(individualA) should be (false)
    individualA.hasBetterFitness(individualA) should be (false)
  }

  test("hasBetterFitness.SortWith1") {
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    val causalMatrixA = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
    )
    val individualA = new Individual(causalMatrixA, eventLog, "x", "y")

    val causalMatrixB = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
    )

    val individualB = new Individual(causalMatrixB, eventLog, "x", "y")

    val individualC = new Individual(causalMatrixA, eventLog, "x", "y") //c is equal to a

    val sorted = List(individualB, individualA, individualC).sortWith((a, b) => a.hasBetterFitness(b))

    sorted should be (List(individualA, individualC, individualB))
    sorted should be (List(individualC, individualA, individualB))

  }

  test("hasBetterFitness.SortWith2") {
    val eventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y")
    )
    val causalMatrixA = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"), Place("d"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"), Place("e"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
    )
    val individualA = new Individual(causalMatrixA, eventLog, "x", "y")

    val causalMatrixB = mutable.Map(
      "x" -> Edges(Places(), Places(Place("b", "a"))),
      "a" -> Edges(Places(Place("x")), Places(Place("c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c"))),
      "c" -> Edges(Places(Place("b", "a")), Places(Place("e", "d"))),
      "d" -> Edges(Places(Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("e", "d")), Places())
    )
    val individualB = new Individual(causalMatrixB, eventLog, "x", "y")

    val causalMatrixC = mutable.Map(
      "x" -> Edges(Places(), Places(Place("a", "b"))),
      "a" -> Edges(Places(Place("x")), Places(Place("d", "c"))),
      "b" -> Edges(Places(Place("x")), Places(Place("c", "e"))),
      "c" -> Edges(Places(Place("a"), Place("b")), Places(Place("d", "e"))),
      "d" -> Edges(Places(Place("a"), Place("c")), Places(Place("y"))),
      "e" -> Edges(Places(Place("b"), Place("c")), Places(Place("y"))),
      "y" -> Edges(Places(Place("d"), Place("e")), Places())
    )
    val individualC = new Individual(causalMatrixC, eventLog, "x", "y")

    val sorted = List(individualC, individualA, individualB).sortWith((a, b) => a.hasBetterFitness(b))
    sorted should be (List(individualA, individualB, individualC))

    List(individualA, individualA, individualB).sortWith((a, b) => a.hasBetterFitness(b)) should be (List(individualA, individualA, individualB))
    List(individualA, individualB, individualA).sortWith((a, b) => a.hasBetterFitness(b)) should be (List(individualA, individualA, individualB))
  }

  test("hasBetterFitness.IsEqualTest") {
    val eventLog = EventLog()
    val causalMatrix: mutable.Map[String, Edges] = mutable.Map()

    val individualA = new Individual(causalMatrix, eventLog, "a", "g", calculateFitnessValuesAtStart = false)
    val individualB = new Individual(causalMatrix, eventLog, "a", "g", calculateFitnessValuesAtStart = false)
    individualA.setFitnessValues(100000.1, 1.34, 1.56)
    individualB.setFitnessValues(100000.0, 2.34, 1.56)

    individualA.hasBetterFitness(individualB) should be (false)
    individualB.hasBetterFitness(individualA) should be (true)
  }


}
