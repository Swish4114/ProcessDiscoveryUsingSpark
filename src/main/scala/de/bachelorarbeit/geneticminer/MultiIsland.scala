package de.bachelorarbeit.geneticminer

import de.bachelorarbeit.logging.Logger
import org.apache.spark.rdd.RDD
import org.apache.spark.{HashPartitioner, SparkConf, SparkContext}

import scala.io.Source

class MultiIsland(final val hdfsEventLogPath: String, final val NUMBER_OF_CYCLES: Int) {
  final val conf: SparkConf = new SparkConf().setAppName("GeneticMiner")
  conf.set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
  conf.set("spark.kryoserializer.buffer.max", "1024m")
  conf.registerKryoClasses(Array(classOf[Trace], classOf[CausalMatrix], classOf[Place], classOf[Places]))
  conf.setIfMissing("spark.master", "local")

  Logger.info(s"number of executors: ${conf.get("spark.executor.instances")} number of cores: ${conf.get("spark.executor.cores")} " +
                s"driver memory: ${conf.get("spark.driver.memory")} executors memory: ${conf.get("spark.executor.memory")}")

  final val isLocal: Boolean = conf.get("spark.master").startsWith("local")
  final val sc: SparkContext = new SparkContext(conf)
  sc.setLogLevel("WARN")
  if (!isLocal) sc.setCheckpointDir("hdfs://zeus.ielc:8020/user/xyz/checkpointdir")

  def mine(): Unit = {
    val NUM_PARTITIONS = 21
    val PS = 100
    val MI = 5
    val MS = (0.1 * PS).toInt //10% of PS
    val CROSSOVER_RATE = 0.8
    val P = 18
    val INITIAL_TIMES_RUN = 35

    val hashPartitioner = new HashPartitioner(NUM_PARTITIONS)

    val timesRunAccumulators: Seq[org.apache.spark.util.LongAccumulator] = (0 until NUM_PARTITIONS).map(i => {
      val accumulator = sc.longAccumulator(s"timesRun Accumolator for island: $i")
      accumulator.add(INITIAL_TIMES_RUN)
      accumulator
    })

    val rawEventLogRDD: RDD[String] = sc.textFile(hdfsEventLogPath, NUM_PARTITIONS).filter(line => !line.isEmpty)//.repartition(NUM_PARTITIONS)
    val eventLogRDD: RDD[(Int, Trace)] = rawEventLogRDD.mapPartitionsWithIndex({ case (index, iterator) =>
      iterator.map(line => (index, line.split(" ").toVector))
    }).partitionBy(hashPartitioner).persist()


    Logger.info(s"UsedPartitions: ${rawEventLogRDD.getNumPartitions} $NUM_PARTITIONS. SourceFile: $hdfsEventLogPath, NumberGenerations: $NUMBER_OF_CYCLES, " +
                s"PS: $PS, MI: $MI, MS: $MS, crossoverRate: $CROSSOVER_RATE, P: $P")

    var individualRDD: RDD[(Int, RawIndividual)] = sc.emptyRDD[(Int, RawIndividual)].partitionBy(hashPartitioner)

    for (i <- 1 to NUMBER_OF_CYCLES) {
      Logger.info(s"####run $i####")

      val timesRunValues: Seq[Int] = timesRunAccumulators.map(accu => accu.sum.toInt)

      /*#################################
        ###Mining phase on the islands###
        #################################*/
      val islandRdd: RDD[(Int, (Iterable[Trace], Iterable[RawIndividual]))] = eventLogRDD.cogroup(individualRDD)
      individualRDD = islandRdd.mapPartitionsWithIndex({ case (index, iterator) =>
        val (key, (traces, rawIndividuals)) = iterator.next()
        val eventLog = EventLog(traces.toSeq: _*)

        val currentPopulation: Seq[Individual] = rawIndividuals.map(rawIndividual => {
          val individual = new Individual(rawIndividual._1, eventLog, rawIndividual._2, rawIndividual._3, calculateFitnessValuesAtStart = false)
          individual.setFitnessValues(rawIndividual._4, rawIndividual._5, rawIndividual._6)
          individual
        }).toSeq

        val island = new Island(eventLog, POPULATION_SIZE = PS, CROSSOVER_RATE = CROSSOVER_RATE, P = P, islandIndex = index)
        val currentTimesRun = timesRunValues(index)
        val (newCurrentPopulation, newTimesRun) = island.mineAsIsland(MAX_GENERATIONS = MI, INITIAL_TIMES_RUN = INITIAL_TIMES_RUN,
                                                                      currentTimesRun = currentTimesRun, currentPopulation: _*)
        timesRunAccumulators(index).add(-currentTimesRun + newTimesRun)

        newCurrentPopulation.map(individual => (index,
          (individual.causalMatrix, individual.startTask, individual.endTask, individual.getCompleteness,
            individual.getPrecision, individual.getSimplicity))).toIterator
      }, preservesPartitioning = true).persist()


      /*#####################################################################
        ###Selecting the best n individuals to send to the neighbour island###
        #####################################################################*/
      val selectionRDD: RDD[(Int, RawIndividual)] = individualRDD.mapPartitionsWithIndex({ case (index, currentPopulation) =>
        def getRandom: Int = {
          val r = scala.util.Random.nextInt(NUM_PARTITIONS)
          if (r != index)
            r
          else
            getRandom
        }
        //val migrationIsland = (index + 1) % NUM_PARTITIONS
        val migrationIsland = getRandom
        val bestIndividuals = currentPopulation.take(MS)
        val best = bestIndividuals.map(bestIndividual => {
          assert(index == bestIndividual._1)
          (migrationIsland, bestIndividual._2)
        })
        Logger.info(s"island: $index. Took $MS from $index to $migrationIsland best individual.")
        best
      }, preservesPartitioning = false).partitionBy(hashPartitioner)


      /*##################################################################################
        ###merging the received individuals with current individuals                   ###
        ##################################################################################*/
      val mergingRDD: RDD[(Int, (Iterable[RawIndividual], Iterable[RawIndividual]))] = individualRDD.cogroup(selectionRDD)
      individualRDD = mergingRDD.mapPartitionsWithIndex({ case (index, iterator) =>
        val (key, (currentPopulationIterable, selectedIndividuals)) = iterator.next()
        assert(key == index)
        val newPopulation = selectedIndividuals ++ currentPopulationIterable.take(PS - selectedIndividuals.size)
        Logger.info(s"island: $index. Merged. Selection Size = ${selectedIndividuals.size}, New Pop Size = ${newPopulation.size}")
        newPopulation.map(individual => (index, individual)).toIterator
      }, preservesPartitioning = true)


      if (i % 20 == 0 && !isLocal) {
        //https://github.com/JerryLead/SparkInternals/blob/master/markdown/english/6-CacheAndCheckpoint.md
        individualRDD.persist()
        individualRDD.checkpoint()
      }
      individualRDD.count() //ok?
    }

    val result: Array[(Int, RawIndividual)] = individualRDD.mapPartitions(iterator => iterator.take(10)).collect()
    //val result: Array[(Int, RawIndividual)] = individualRDD.collect()
    val eventLog = EventLog(eventLogRDD.map(pair => pair._2).collect(): _*)
    println(s"eventLogLength: ${eventLog.size}")
    Logger.info("####result####")
    result.foreach(pair => {
      val key = pair._1
      val rawIndividual = pair._2
      Logger.info(key, rawIndividual._4, rawIndividual._5, rawIndividual._6)
      val individual = new Individual(rawIndividual._1, eventLog, rawIndividual._2, rawIndividual._3)
      Logger.info(individual.prettyStringFitnessMeasures)
    })

    if (isLocal)
      System.in.read()
  }
}

object MultiIsland {
  def parseFile(fPath: String): EventLog = {
    val source = Source.fromFile(fPath)
    val lines = source.getLines.map(line => line.split(" ").toVector).toSeq
    val log = EventLog(lines: _*)
    source.close()
    log
  }


  def main(args: Array[String]): Unit = {
    val fPath = "hdfs://zeus.ielc:8020/user/xyz/" + args(0)
    val numGenerations = args(1).toInt
    val miner = new MultiIsland(fPath, numGenerations)
    miner.mine()
  }
}
