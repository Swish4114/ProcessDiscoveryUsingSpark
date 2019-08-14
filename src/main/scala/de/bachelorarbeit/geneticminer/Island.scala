package de.bachelorarbeit.geneticminer

import java.util.Calendar

import de.bachelorarbeit.logging.{LogLevel, Logger}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class Island(final val eventLog: EventLog, final val POPULATION_SIZE: Int, final val P: Int, final val CROSSOVER_RATE: Double,
             final val islandIndex: Int = 0) {

  private final val activities: Vector[Task] = eventLog.activities

  private val startTask = eventLog.map(trace => trace.head).groupBy(identity).maxBy(_._2.size)._1
  private val endTask = eventLog.map(trace => trace.last).groupBy(identity).maxBy(_._2.size)._1

  final val randomObject = scala.util.Random

  final val heuristicMiner = new HeuristicMiner(eventLog, 0.9, 0.1, 0.05)

  def createInitialIndividual(): Individual = {
    val numActivities = activities.size
    val p = randomObject.nextInt(math.max(1, P / 2)) * 2 + 1

    val outputs = mutable.Map[Task, ListBuffer[(Int, Task)]]()
    val inputs = mutable.Map[Task, ListBuffer[(Int, Task)]]()
    for (a <- activities; b <- activities) {
      if (!outputs.contains(a))
        outputs(a) = ListBuffer[(Int, Task)]()
      if (!inputs.contains(b))
        inputs(b) = ListBuffer[(Int, Task)]()

      val r = randomObject.nextDouble()
      val depMeasure = eventLog.getDependencyMeasure(a, b)

      if (r  < scala.math.pow(depMeasure, p)) {
        outputs(a).append((randomObject.nextInt(numActivities), b))
        inputs(b).append((randomObject.nextInt(numActivities), a))
      }
    }
    val causalMatrix = CausalMatrix()
    for (a <- activities) {
      val outputPlaces = Places()
      outputs(a).groupBy(pair => pair._1).foreach({case (_, value) =>
        val tasks = value.map(pair => pair._2)
        outputPlaces.append(Place(tasks: _*))
      })
      val inputPlaces = Places()
      inputs(a).groupBy(pair => pair._1).foreach({case (_, value) =>
        val tasks = value.map(pair => pair._2)
        inputPlaces.append(Place(tasks: _*))
      })
      causalMatrix(a) = Edges(inputPlaces, outputPlaces)
    }
    val individual = new Individual(causalMatrix, eventLog, startTask, endTask)
    individual.seenIslands += islandIndex
    individual
  }

  def createInitialPopulationSingleThreaded(individualsToAdd: Individual*): Individuals = {
    val individuals = (individualsToAdd.size until POPULATION_SIZE).map(_ => createInitialIndividual())
    val result = individualsToAdd ++ individuals
    result.toIndexedSeq
  }

  def createInitialPopulation(individualsToAdd: Individual*): Individuals = {
    Logger.info(s"island: $islandIndex. Creating initial population")
    val individuals = (individualsToAdd.size until POPULATION_SIZE).par.map(i => {
      val individual = createInitialIndividual()
      individual
    })
    val result = individualsToAdd ++ individuals
    Logger.info(s"island: $islandIndex. Done Creating initial population")
    result.toIndexedSeq
  }

  def swapPlaces(swapPlaces: Places, remainingPlaces: Places): Unit = {
    swapPlaces.foreach(swapPlace => {
      randomObject.nextInt(3) match {
        case 0 => remainingPlaces.append(swapPlace)
        case 1 if remainingPlaces.nonEmpty => remainingPlaces(randomObject.nextInt(remainingPlaces.size)) ++= swapPlace
        case 2 =>
          if (remainingPlaces.nonEmpty) {
            val randomIndex = randomObject.nextInt(remainingPlaces.size)
            val randomPlace = remainingPlaces(randomIndex)
            randomPlace --= swapPlace
            if (randomPlace.isEmpty)
              remainingPlaces.remove(randomIndex)
          }
          remainingPlaces.append(swapPlace)
        case _ =>
      }
    })
  }

  def swapForCrossover(places1: Places, places2: Places): (Places, Places) = {
    val swapPoint1 = randomObject.nextInt(math.max(1, places1.size))
    val swapPoint2 = randomObject.nextInt(math.max(1, places2.size))

    val remainingPlaces1 = places1.slice(0, swapPoint1)
    val swapPlaces1 = places1.slice(swapPoint1, places1.size)

    val remainingPlaces2 = places2.slice(0, swapPoint2)
    val swapPlaces2 = places2.slice(swapPoint2, places2.size)

    swapPlaces(swapPlaces2, remainingPlaces1)
    swapPlaces(swapPlaces1, remainingPlaces2)

    (remainingPlaces1, remainingPlaces2)
  }

  def getIncorrectlyFiredActivitiesSet(individual1: Individual, individual2: Individual): IndexedSeq[Task] = {
    val selected = {
      if (individual1.hasBetterFitness(individual2))
        individual1
      else
        individual2
    }

    val incorrectlyFiredActivtiesSet = eventLog
      .flatMap(trace => {
        val (_, missingTasks, tasksThatHaveLeftPlacesMarked, _) = selected.tokenReplay(trace)
        missingTasks ++ tasksThatHaveLeftPlacesMarked
      })
    incorrectlyFiredActivtiesSet.distinct.toIndexedSeq
  }

  def crossoverTask(crossoverPoint: Task, individual1: Individual, individual2: Individual): Unit = {
    //first doing crossover for the input edges
    val input1 = individual1.getInput(crossoverPoint)
    val input2 = individual2.getInput(crossoverPoint)
    val oldInput1Tasks = Individual.convertPlacesToListOfTask(input1)
    val oldInput2Tasks = Individual.convertPlacesToListOfTask(input2)
    val (newInput1, newInput2) = swapForCrossover(input1, input2)
    individual1.setInput(crossoverPoint, newInput1)
    individual2.setInput(crossoverPoint, newInput2)
    individual1.repairOutput(crossoverPoint, oldInput1Tasks)
    individual2.repairOutput(crossoverPoint, oldInput2Tasks)

    //now doing crossover for their output edges
    val output1 = individual1.getOutput(crossoverPoint)
    val output2 = individual2.getOutput(crossoverPoint)
    val oldOutput1Tasks = Individual.convertPlacesToListOfTask(output1)
    val oldOutput2Tasks = Individual.convertPlacesToListOfTask(output2)
    val (newOutput1, newOutput2) = swapForCrossover(output1, output2)
    individual1.setOutput(crossoverPoint, newOutput1)
    individual2.setOutput(crossoverPoint, newOutput2)
    individual1.repairInput(crossoverPoint, oldOutput1Tasks)
    individual2.repairInput(crossoverPoint, oldOutput2Tasks)
  }

  def crossover(individual1: Individual, individual2: Individual): Option[Task] = {
    val r = randomObject.nextDouble()
    if (r >= CROSSOVER_RATE)
      return None

    val incorrectlyFiredActivtiesSequence = getIncorrectlyFiredActivitiesSet(individual1, individual2)

    val crossoverPoint = {
      if (incorrectlyFiredActivtiesSequence.nonEmpty)
        incorrectlyFiredActivtiesSequence(randomObject.nextInt(incorrectlyFiredActivtiesSequence.size))
      else
        activities(randomObject.nextInt(activities.size))
    }

    crossoverTask(crossoverPoint, individual1, individual2)

    Some(crossoverPoint)
  }


  def tournamentSelection(currentPopulation: Individuals): Individuals = {
    (0 until POPULATION_SIZE).map(_ => {
      val individual1 = currentPopulation(randomObject.nextInt(POPULATION_SIZE))
      val individual2 = currentPopulation(randomObject.nextInt(POPULATION_SIZE))
      if (individual1 hasBetterFitness individual2)
        individual1
      else
        individual2
    })
  }

  def replacement(currentPopulation: Individuals, offsprings: Individuals): Individuals = {
    val allIndividuals = offsprings ++ currentPopulation
    val distinct = allIndividuals.distinct.sortWith((a, b) => a.hasBetterFitness(b))
    val repeated = allIndividuals.diff(distinct).sortWith((a, b) => a.hasBetterFitness(b))
    val sortedIndividuals = distinct ++ repeated
    sortedIndividuals.take(POPULATION_SIZE).toIndexedSeq
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def mineLocal(MAX_GENERATIONS: Int, INITIAL_TIMES_RUN: Int, MAX_RESTARTS: Int): Individuals = {
    Logger.info(s"Starting mining. POPULATION_SIZE=$POPULATION_SIZE, CROSSOVER_RATE=$CROSSOVER_RATE, " +
      s"P=$P, NUM_RUNS=$MAX_GENERATIONS, INITIAL_TIMES_RUN=$INITIAL_TIMES_RUN, MAX_RESTARTS=$MAX_RESTARTS")
    val initialPopulation = {
      val individual = heuristicMiner.getIndividual
      Logger.info(s"added heuristic individual: ${individual.prettyStringFitnessMeasures}")
      createInitialPopulation(individual)
    }
    var currentPopulation = initialPopulation
    var t = 0
    var timesRun = INITIAL_TIMES_RUN
    var restarts = 0
    val range = 0.until(POPULATION_SIZE).by(2).par

    while (t < MAX_GENERATIONS && restarts < MAX_RESTARTS) {
      val selection = tournamentSelection(currentPopulation)
      val offsprings = range.flatMap(i => {
        val offspring1 = selection(i).copy()
        val offspring2 = {
          if (i < selection.length - 1)
            selection(i + 1).copy()
          else
            selection(0).copy()
        }

        val crosssoverTask = crossover(offspring1, offspring2)
        val mutationTask1 = offspring1.mutate()
        offspring1.calculateFitnessValues()
        val mutationTask2 = offspring2.mutate()
        offspring2.calculateFitnessValues()
        List(offspring1, offspring2)
      }).toIndexedSeq
      val oldPopulation = currentPopulation
      currentPopulation = replacement(currentPopulation, offsprings)

      t += 1
      val bestIndividual = currentPopulation(0)
      Logger.info(s"Generation $t finished. Best Individual:\n${bestIndividual.prettyStringFitnessMeasures}")

      if (Island.noNewIndividuals(currentPopulation, oldPopulation)) {
        timesRun -= 1
      }
      if (Island.areIndividualsEqual(oldPopulation.head, currentPopulation.head)) {
        timesRun -= 1
      }
      if (timesRun < 0) {
        restarts += 1
        if (restarts < MAX_RESTARTS)
          currentPopulation = createInitialPopulation(currentPopulation.head)
        timesRun = INITIAL_TIMES_RUN
        Logger.info(s"Reinit $restarts happened")
      }
    }
    currentPopulation
  }

  def mineAsIsland(MAX_GENERATIONS: Int, INITIAL_TIMES_RUN: Int, currentTimesRun: Int, existingIndividuals: Individual*): (Individuals, Int) = {
    Logger.info(s"island: $islandIndex. Starting mining. POPULATION_SIZE=$POPULATION_SIZE, CROSSOVER_RATE=$CROSSOVER_RATE, " +
      s"P=$P, NUM_RUNS=$MAX_GENERATIONS, NUM_INITIAL_INDIVIDUALS: ${existingIndividuals.size}, EVENT_LOG_SIZE: ${eventLog.size}, currentTimesRun: $currentTimesRun, initialTimesRun: $INITIAL_TIMES_RUN")
    val initialPopulation = {
      if (existingIndividuals.nonEmpty)
        createInitialPopulation(existingIndividuals: _*)
      else {
        val individual = heuristicMiner.getIndividual
        Logger.info(s"added heuristic individual: ${individual.prettyStringFitnessMeasures}")
        createInitialPopulation(individual)
      }
    }
    var currentPopulation = initialPopulation
    var t = 0
    var timesRun = currentTimesRun
    var restarts = 0
    val range = 0.until(POPULATION_SIZE).by(2).par

    while (t < MAX_GENERATIONS) {
      val selection = tournamentSelection(currentPopulation)
      val offsprings = range.flatMap(i => {
        val offspring1 = selection(i).copy()
        val offspring2 = {
          if (i < selection.length - 1)
            selection(i + 1).copy()
          else
            selection(0).copy()
        }

        val crosssoverTask = crossover(offspring1, offspring2)
        val mutationTask1 = offspring1.mutate()
        offspring1.calculateFitnessValues()
        val mutationTask2 = offspring2.mutate()
        offspring2.calculateFitnessValues()
        List(offspring1, offspring2)
      }).toIndexedSeq
      val oldPopulation = currentPopulation
      currentPopulation = replacement(currentPopulation, offsprings)

      t += 1
      Logger.info(s"island: $islandIndex. Generation $t finished. Best individual: ${currentPopulation(0).prettyStringFitnessMeasures}")

      if (Island.noNewIndividuals(currentPopulation, oldPopulation)) {
        timesRun -= 1
        Logger.info(s"island: $islandIndex. no new individuals. timesRun: $timesRun")
      }
      if (Island.areIndividualsEqual(oldPopulation.head, currentPopulation.head)) {
        timesRun -= 1
        Logger.info(s"island: $islandIndex. no new best individual. timesRun: $timesRun")
      }
      if (timesRun < 0) {
        restarts += 1
        currentPopulation = createInitialPopulation(currentPopulation.head)
        timesRun = INITIAL_TIMES_RUN
        Logger.info(s"island: $islandIndex. Reinit $restarts happened")
      }
    }
    (currentPopulation, timesRun)
  }
}

object Island {

  def threadSafeSet[T](): mutable.Set[T] = { //stolen from https://stackoverflow.com/a/40994099
    import scala.collection.JavaConverters._
    java.util.Collections.newSetFromMap(
      new java.util.concurrent.ConcurrentHashMap[T, java.lang.Boolean]).asScala
  }

  def noNewIndividuals(newPopulation: Individuals, oldPopulation: Individuals): Boolean = {
    newPopulation.diff(oldPopulation).isEmpty
  }

  def areIndividualsEqual(i1: Individual, i2: Individual): Boolean = {
    i1 == i2
  }

  def parseFile(fPath: String): EventLog = {
    val source = Source.fromFile(fPath)
    val lines = source.getLines.map(line => line.split(" ").toVector).toSeq
    val log = EventLog(lines: _*)
    source.close()
    log
  }

  def main(args: Array[String]): Unit = {
    val proDiGenEventLog = EventLog(
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "b", "c", "e", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y"),
      Trace("x", "a", "c", "d", "y")
    )

    Logger.setLogLevel(LogLevel.FINE)

    val proDiGen = new Island(proDiGenEventLog, POPULATION_SIZE = 100, CROSSOVER_RATE = 0.8, P = 10)
    val result = proDiGen.mineLocal(MAX_GENERATIONS = 1000, INITIAL_TIMES_RUN = 35, MAX_RESTARTS = 5)
    result.take(5).foreach(println)
  }
}