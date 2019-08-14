package de.bachelorarbeit.geneticminer

class HeuristicMiner(final val eventLog: EventLog,
                     final val DEPENDENCY_MEASURE_THRESHOLD: Double,
                     final val AND_MEASURE_THRESHOLD: Double,
                     final val RELATIVE_TO_BEST_THRESHOLD: Double) {

  val causalMatrix: CausalMatrix = CausalMatrix()
  val startTask: Task = eventLog.map(trace => trace.head).groupBy(identity).maxBy(_._2.size)._1
  val endTask: Task = eventLog.map(trace => trace.last).groupBy(identity).maxBy(_._2.size)._1
  mine()

  def andLinkMeasureOutput(a: Task, b: Task, c: Task): Double = { //see Process Mining with the HeuristicsMiner Algorithm Page 11
    val numberAB = eventLog.numberAB
    val b_c = if (numberAB.contains(b, c)) numberAB(b, c).toDouble else 0
    val c_b = if (numberAB.contains(c, b)) numberAB(c, b).toDouble else 0
    val a_b = if (numberAB.contains(a, b)) numberAB(a, b).toDouble else 0
    val a_c = if (numberAB.contains(a, c)) numberAB(a, c).toDouble else 0

    (b_c + c_b) / (a_b + a_c + 1)
  }

  def andLinkMeasureInput(a: Task, b: Task, c: Task): Double = {
    val numberAB = eventLog.numberAB
    val b_c = if (numberAB.contains(b, c)) numberAB(b, c).toDouble else 0
    val c_b = if (numberAB.contains(c, b)) numberAB(c, b).toDouble else 0
    val b_a = if (numberAB.contains(b, a)) numberAB(b, a).toDouble else 0
    val c_a = if (numberAB.contains(c, a)) numberAB(c, a).toDouble else 0

    (b_c + c_b) / (b_a + c_a + 1)
  }

  def getMaximumDependencyMeasure: Double = {
    var max = Double.MinValue
    for (a <- eventLog.activities; b <- eventLog.activities) {
      val dependencyMeasure = eventLog.getDependencyMeasure(a, b)
      if (dependencyMeasure > max) max = dependencyMeasure
    }
    max
  }

  private def minePlaces(a: Task, existingPlaces: Places, bestDependencyMeasure: Double,
                         dependencyMeasure: (Task, Task) => Double,
                         andLinkMeasure: (Task, Task, Task) => Double): Unit = {
    val dependencies = eventLog.activities.filter(b => {
      val depMeasure = dependencyMeasure(a, b)
      (depMeasure > DEPENDENCY_MEASURE_THRESHOLD) && ((bestDependencyMeasure - depMeasure) <= RELATIVE_TO_BEST_THRESHOLD)
    })
    dependencies.size match {
      case x if x > 1 =>
        val uniquePairs = for { //stolen from https://stackoverflow.com/a/45725255
          (x, idxX) <- dependencies.zipWithIndex
          (y, idxY) <- dependencies.zipWithIndex
          if idxX < idxY
        } yield (x, y)
        for (pair <- uniquePairs) {
          val b = pair._1
          val c = pair._2
          if (andLinkMeasure(a, b, c) > AND_MEASURE_THRESHOLD) {
            // it is an and link
            var containsB, containsC = false
            for (place <- existingPlaces if !containsB || !containsC) {
              if (place.contains(b)) containsB = true
              if (place.contains(c)) containsC = true
            }
            if (!containsB) existingPlaces.append(Place(b))
            if (!containsC) existingPlaces.append(Place(c))
          } else {
            //it is an or link
            var added = false
            for (place <- existingPlaces if !added) {
              if (place.contains(b)) {
                if (!place.contains(c)) place.add(c)
                added = true
              } else if (place.contains(c)) {
                if (place.contains(b)) place.add(b)
                added = true
              }
            }
            if (!added) existingPlaces.append(Place(b, c))
          }
        }
      case x if x == 1 =>
        val b = dependencies(0)
        existingPlaces.append(Place(b))
      case _ =>
    }
  }

  private def mine(): Unit = {
    var bestDependencyMeasure = Double.MinValue
    for (a <- eventLog.activities; b <- eventLog.activities) {
      val ab = eventLog.getDependencyMeasure(a, b)
      val ba = eventLog.getDependencyMeasure(b, a)
      if (ab > bestDependencyMeasure)
        bestDependencyMeasure = ab
      if (ba > bestDependencyMeasure)
        bestDependencyMeasure = ba
    }

    for (a <- eventLog.activities) {
      val inputs, outputs = Places()
      causalMatrix(a) = Edges(inputs, outputs)
      minePlaces(a, outputs, bestDependencyMeasure, (a: Task, b: Task) => eventLog.getDependencyMeasure(a, b), (a: Task, b: Task, c: Task) => andLinkMeasureOutput(a, b, c))
      minePlaces(a, inputs, bestDependencyMeasure, (a: Task, b: Task) => eventLog.getDependencyMeasure(b, a), (a: Task, b: Task, c: Task) => andLinkMeasureInput(a, b, c))
    }
  }

  def getIndividual: Individual = new Individual(causalMatrix, eventLog, startTask, endTask)
}
