package de.bachelorarbeit.geneticminer

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Individual(final val causalMatrix: CausalMatrix,
                 final val eventLog: EventLog,
                 final val startTask: Task,
                 final val endTask: Task,
                 private val calculateFitnessValuesAtStart: Boolean = true) {

  final val randomObject = scala.util.Random
  private var completeness, precision, simplicity = 0.0
  if (calculateFitnessValuesAtStart) calculateFitnessValues()

  val seenIslands: ListBuffer[Int] = ListBuffer[Int]()

  def copy(): Individual = {
    val copyCausalMatrix = CausalMatrix()
    causalMatrix.foreach({case (task, edges) =>
      val copyEdges = Edges(edges.inputs.map(place => place.clone()),
                            edges.outputs.map(place => place.clone()))
      copyCausalMatrix(task) = copyEdges
    })
    val copyIndividual = new Individual(copyCausalMatrix, eventLog, startTask, endTask, calculateFitnessValuesAtStart = false)
    copyIndividual.completeness = completeness
    copyIndividual.precision = precision
    copyIndividual.simplicity = simplicity
    copyIndividual.seenIslands ++= seenIslands
    copyIndividual
  }

  def getInput(task: Task): Places = {
    if (causalMatrix.contains(task))
      return causalMatrix(task).inputs
    Places()
  }

  def getOutput(task: Task): Places = {
    if (causalMatrix.contains(task))
      return causalMatrix(task).outputs
    Places()
  }

  def setInput(task: Task, places: Places): Unit = {
    if (causalMatrix.contains(task)) {
      causalMatrix(task).inputs = places
    } else {
      causalMatrix(task) = Edges(places, Places())
    }
  }
  def setOutput(task: Task, places: Places): Unit = {
    if (causalMatrix.contains(task)) {
      causalMatrix(task).outputs = places
    } else {
      causalMatrix(task) = Edges(Places(), places)
    }
  }

  def getCompleteness: Double = completeness
  def getPrecision: Double = precision
  def getSimplicity: Double = simplicity

  def setFitnessValues(completeness: Double, precision: Double, simplicity: Double): Unit = {
    this.completeness = completeness
    this.precision = precision
    this.simplicity = simplicity
  }

  def calculateFitnessValues(): Unit = {
    calcCompletenessAndPrecision()
    calcSimplicity()
  }

  def repairOutput(taskToRepair: Task, oldInputTasks: List[Task]): Unit = {
    Individual.repair(taskToRepair, oldInputTasks, getOutput, getInput, randomObject)
  }

  def repairInput(taskToRepair: Task, oldOutputTasks: List[Task]): Unit = {
    Individual.repair(taskToRepair, oldOutputTasks, getInput, getOutput, randomObject)
  }

  private def isPlaceMarked(markedPlaces: mutable.HashMap[Task, mutable.HashMap[Place, Int]],
                            place: Place,
                            taskThatShouldContain: Task,
                            alsoDecrementAndRemove: Boolean): Boolean = {
    for (task <- place if markedPlaces.contains(task)) {
      val markedPlacesOfTask: mutable.Map[Place, Int] = markedPlaces(task)
      for ((markedPlace, _) <- markedPlacesOfTask if markedPlace.contains(taskThatShouldContain)) {
        if (alsoDecrementAndRemove) {
          markedPlacesOfTask(markedPlace) -= 1
          if (markedPlacesOfTask(markedPlace) == 0)
            markedPlacesOfTask -= markedPlace
        }
        return true
      }
    }
    false
  }

  private def getNumMarkedPlaces(markedPlaces: mutable.HashMap[Task, mutable.HashMap[Place, Int]],
                                 currentTask: Task,
                                 alsoDecrementAndRemove: Boolean): Int = {
    val inputs = getInput(currentTask)
    val numInputs = inputs.size
    var numInputsMarked = 0
    for (inputPlace <- inputs) {
      if (isPlaceMarked(markedPlaces, inputPlace, currentTask, alsoDecrementAndRemove))
        numInputsMarked += 1
    }
    numInputsMarked
  }

  def tokenReplay(trace: Trace): (List[Task], List[Task], List[Task], Int) = {
    val FAKE_START_PLACE_TASK = "0"
    val correctTasks = ListBuffer[Task]()
    val missingTasks = ListBuffer[Task]()
    val markedPlaces = mutable.HashMap[Task, mutable.HashMap[Place, Int]]()
    markedPlaces(FAKE_START_PLACE_TASK) = mutable.HashMap((Place(startTask), 1))

    var numEnabledTasks = 1 //1 -> The Start node is always enabled

    var startTokenParsed = false
    var endTaskParsed = false

    for (task <- trace if causalMatrix.contains(task)) {
      if (!startTokenParsed && task == startTask) {
        correctTasks.append(task)
        markedPlaces -= FAKE_START_PLACE_TASK
        startTokenParsed = true
      } else {
        val numInputs = getInput(task).size
        val numInputsMarked = getNumMarkedPlaces(markedPlaces, task, alsoDecrementAndRemove = true)
        if ((numInputs == numInputsMarked) && (numInputs > 0)) {
          correctTasks.append(task)
        } else {
          val numMissingTasks = math.max(1, numInputs - numInputsMarked) //If the task has no input place (so its floating), we count at least 1 task
          var i = 0
          while (i < numMissingTasks) {
            missingTasks.append(task)
            i += 1
          }
        }
      }

      val outputPlaces = getOutput(task)
      if (outputPlaces.nonEmpty) {
        outputPlaces.foreach(place => {
          if (!markedPlaces.contains(task)) markedPlaces(task) = mutable.HashMap[Place, Int]()
          val map = markedPlaces(task)
          if (!map.contains(place)) map(place) = 0
          map(place) += 1
        })
      } else {
        if (endTask != task) {
          if (!markedPlaces.contains(task)) markedPlaces(task) = mutable.HashMap[Place, Int]()
          val map: mutable.Map[Place, Int] = markedPlaces(task)
          val place = Place(task)
          if (!map.contains(place)) map(place) = 0
          map(place) += 1
        } else {
          endTaskParsed = true
        }
      }

      val alreadySeen = mutable.HashSet[Task]()
      for (place <- outputPlaces) {
        for (t <- place) {
          if (!alreadySeen.contains(t)) {
            val numInputs = getInput(t).size
            if (getNumMarkedPlaces(markedPlaces, t, alsoDecrementAndRemove = false) == numInputs)
              numEnabledTasks += 1
            alreadySeen.add(t)
          }
        }
      }
    }
    if (!endTaskParsed)
      missingTasks.append(endTask)

    val tasksThatHaveLeftPlacesMarked = ListBuffer[Task]()
    for ((task, places) <- markedPlaces) {
      for ((_, numMarked) <- places) {
        var i = 0
        while (i < numMarked) {
          tasksThatHaveLeftPlacesMarked += task
          i += 1
        }
      }
    }
    (correctTasks.toList, missingTasks.toList, tasksThatHaveLeftPlacesMarked.toList, numEnabledTasks)
  }

  private def tokenReplayResultToNumbers(trace: Trace): (Int, Int, Int, Int) = {
    val tokenReplayResult = tokenReplay(trace)
    (tokenReplayResult._1.size, tokenReplayResult._2.size, tokenReplayResult._3.size, tokenReplayResult._4)
  }

  private def calcCompletenessAndPrecision(): Unit = {
    val allParsedTasks = new AtomicInteger(0)
    val allMissingTokens = new AtomicInteger(0)
    val allExtraTokensLeftBehind = new AtomicInteger(0)
    val numTracesMissingTokens = new AtomicInteger(0)
    val numTracesExtraTokensLeftBehind = new AtomicInteger(0)
    val numAllEnabledTasks = new AtomicInteger(0)
    eventLog.foreach(trace => {
      val (numParsedTokens, numMissingTokens, numExtraTokensLeftBehind, numEnabledTasks) = tokenReplayResultToNumbers(trace)
      allParsedTasks.addAndGet(numParsedTokens)
      allMissingTokens.addAndGet(numMissingTokens)
      allExtraTokensLeftBehind.addAndGet(numExtraTokensLeftBehind)
      if (numMissingTokens > 0) numTracesMissingTokens.incrementAndGet()
      if (numExtraTokensLeftBehind > 0) numTracesExtraTokensLeftBehind.incrementAndGet()
      numAllEnabledTasks.addAndGet(numEnabledTasks)
    })

    val numTracesLog = eventLog.size
    val punishment = (allMissingTokens.get() / (numTracesLog - numTracesMissingTokens.get() + 1).toDouble) +
                     (allExtraTokensLeftBehind.get() / (numTracesLog - numTracesExtraTokensLeftBehind.get() + 1).toDouble)
    this.completeness = (allParsedTasks.get() - punishment) / eventLog.numAllActivities.toDouble
    this.precision = 1 / numAllEnabledTasks.get().toDouble
  }

  private def calcSimplicity(): Unit = {
    var numTasks = 0
    for ((_, edges) <- causalMatrix) {
      for (place <- edges.inputs) {
        numTasks += place.size
      }
      for (place <- edges.outputs) {
        numTasks += place.size
      }
    }
    this.simplicity = 1 / numTasks.toDouble
  }

  def mutate(): Task = {
    val activities = eventLog.activities
    val taskToMutate = activities(randomObject.nextInt(activities.size))

    val input = getInput(taskToMutate)
    val oldInputTasks = Individual.convertPlacesToListOfTask(input)
    Individual.mutate(input, eventLog.inputDependenciesForMutation(taskToMutate), randomObject)
    repairOutput(taskToMutate, oldInputTasks)

    val output = getOutput(taskToMutate)
    val oldOutputTasks = Individual.convertPlacesToListOfTask(output)
    Individual.mutate(output, eventLog.outputDependenciesForMutation(taskToMutate), randomObject)
    repairInput(taskToMutate, oldOutputTasks)

    taskToMutate
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Individual]

  def hasBetterFitness(other: Individual): Boolean = {
    val completenessThis = getCompleteness
    val completenessOther = other.getCompleteness

    if (Individual.isEqual(completenessThis, completenessOther)) {
      val precisionThis = getPrecision
      val precisionOther = other.getPrecision
      if (Individual.isEqual(precisionThis, precisionOther)) {
        val simplicityThis = getSimplicity
        val simplicityOther = other.getSimplicity
        return simplicityThis > simplicityOther && !Individual.isEqual(simplicityThis, simplicityOther)
      }
      return precisionThis > precisionOther
    }
    completenessThis > completenessOther
  }

  override def equals(other: Any): Boolean = other match {
    case that: Individual =>
      (that canEqual this) &&
        causalMatrix == that.causalMatrix &&
        startTask == that.startTask &&
        endTask == that.endTask
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(causalMatrix, startTask, endTask)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  def prettyStringFitnessMeasures: String = f"Completeness=$getCompleteness%.10f Precision=$getPrecision%.10f Simplicity=$getSimplicity%.10f"

  override def toString: String = {
    val rows = causalMatrix.keys.toList.sorted.map(task => List(
      task match {
        case `startTask` => s"(start) $startTask"
        case `endTask` => s"(end) $endTask"
        case default => default
      },
      causalMatrix(task).inputs.map(set => set.mkString("{", ",", "}")).mkString("{", ",", "}"),
      causalMatrix(task).outputs.map(set => set.mkString("{", ",", "}")).mkString("{", ",", "}")))
    val table = Tabulator.format(List("Activity", "Inputs", "Outputs") :: rows)
    val str = f"$table%s\n$prettyStringFitnessMeasures"
    str
  }
}

object Individual{
  def convertPlacesToListOfTask(places: Places): List[Task] = {
    val tasks = ListBuffer[Task]()
    for (place <- places)
      for (t <- place)
        tasks += t
    tasks.toList
  }

  //stolen from https://www.floating-point-gui.de/errors/comparison/
  def isEqual(a: Double, b: Double): Boolean = {
    val absA = math.abs(a)
    val absB = math.abs(b)
    val diff = math.abs(a - b)
    val epsilon = 1e-6

    if (a == b)
      return true

    if (a == 0 || b == 0 || (absA + absB < java.lang.Double.MIN_NORMAL))
      return diff < (epsilon * java.lang.Double.MIN_NORMAL)

    (diff / math.min(absA + absB, java.lang.Double.MAX_VALUE)) < epsilon
  }

  private def mutate(places: Places, dependencies: IndexedSeq[Task], randomObject: scala.util.Random): Unit = {
    val possibles = ListBuffer[Int]()
    if (dependencies.nonEmpty) possibles += 0
    if (places.nonEmpty) possibles += 1
    if (places.size > 1) possibles += 2
    if (possibles.nonEmpty) {
      var changed = false
      while (!changed) {
        possibles(randomObject.nextInt(possibles.size)) match {
          case 0 =>
            val newTask = dependencies(randomObject.nextInt(dependencies.size))
            val possibles = ListBuffer[Int]()
            if (places.nonEmpty) possibles += 0
            possibles += 1
            possibles(randomObject.nextInt(possibles.size)) match {
              case 0 =>
                val randomPlace = places(randomObject.nextInt(places.size))
                val contains = randomPlace.contains(newTask)
                if (!contains)
                  randomPlace += newTask
                changed = !contains
              case 1 =>
                places += Place(newTask)
                changed = true
            }
          case 1 =>
            val randomIndex = randomObject.nextInt(places.size)
            val randomPlace = places(randomIndex)
            val randomTask = randomPlace.toIndexedSeq(randomObject.nextInt(randomPlace.size))
            randomPlace -= randomTask
            if (randomPlace.isEmpty)
              places.remove(randomIndex)
            changed = true
          case 2 =>
            val hashOfPlacesBefore = places.toSet.hashCode()
            val newPositions = mutable.Map[Int, mutable.Set[Task]]()
            places.foreach(place => {
              place.foreach(task => {
                val randomIndex = randomObject.nextInt(places.size)
                if (!newPositions.contains(randomIndex))
                  newPositions(randomIndex) = mutable.Set[Task]()
                newPositions(randomIndex).add(task)
              })
            })
            val indicesOfPlacesToRemove = ListBuffer[Int]()
            places.zipWithIndex.foreach({ case (place, index) =>
              if (newPositions.contains(index)) {
                place.clear()
                place ++= newPositions(index)
              } else {
                indicesOfPlacesToRemove += index
              }
            })
            indicesOfPlacesToRemove.reverseIterator.foreach(index => places.remove(index))
            changed = places.toSet.hashCode() != hashOfPlacesBefore
        }
      }
    }
  }

  private def repair(taskToRepair: Task, oldTasks: List[Task], func1: Task => Places, func2: Task => Places, randomObject: scala.util.Random): Unit = {
    //TODO: see process mining in a large a tutorial. If task is in old and new than this is not the best solution.
    //For instance O(h) = {{c}, {d}} becomes to O(h) = {{c}, {d}, {g}} then we repair I(c) which must be I(c) = {{b, h}} but it can be now I(c) = {{b}, {h}}. Is that behaviour correct?

    oldTasks.foreach(oldTask => {
      val emptySetsPosition = ListBuffer[Int]()
      val places = func1(oldTask)
      places.zipWithIndex.foreach(pair => {
        if (pair._1.contains(taskToRepair)) pair._1 -= taskToRepair
        if (pair._1.isEmpty) emptySetsPosition.append(pair._2)
      })
      emptySetsPosition.reverseIterator.foreach(i => places.remove(i))
    })

    func2(taskToRepair).flatten.groupBy(identity).mapValues(_.size).foreach(pair => {
      val newTasks = pair._1
      val namOccurrencesThatShouldBe = pair._2
      val places = func1(newTasks)
      val indicesOfPlacesThatDoNotContainTaskToRepair = places.indices.toBuffer
      var i = 0
      while (i < namOccurrencesThatShouldBe) {
        randomObject.nextInt(2) match {
          case 0 if indicesOfPlacesThatDoNotContainTaskToRepair.nonEmpty =>
            val randomIndex = randomObject.nextInt(indicesOfPlacesThatDoNotContainTaskToRepair.size)
            val indexOfPlacesThatDoNotContainTaskToRepair = indicesOfPlacesThatDoNotContainTaskToRepair(randomIndex)
            places(indexOfPlacesThatDoNotContainTaskToRepair) += taskToRepair
            indicesOfPlacesThatDoNotContainTaskToRepair.remove(randomIndex)
          case _ => places += Place(taskToRepair)
        }
        i += 1
      }
    })
  }
}
