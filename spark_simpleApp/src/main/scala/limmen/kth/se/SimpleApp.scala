package limmen.kth.se

import org.apache.log4j.{ Level, LogManager, Logger }
import org.apache.spark.sql._;

/**
  * Simple Spark App
  *
  * Run intructions:
  * 1. package jar with 'sbt package'
  * 2. Submit jar to cluster or local spark: $SPARK_HOME/bin/spark-submit --class "limmen.kth.se.SimpleApp" target/scala-2.11/spark_simpleapp_2.11-0.1.0-SNAPSHOT.jar
  *
  * Alternatively you can simply run from within sbt with 'sbt run' but then you have to live with "InterruptedException"
  */
object SimpleApp {

  val logger = LogManager.getRootLogger
  def main(args: Array[String]) = {

    /*
     * Turn of logging programatically through Log4j API
     */
    Logger.getLogger("org").setLevel(Level.ERROR)
    Logger.getLogger("akka").setLevel(Level.ERROR)

    /*
     * Specify data file
     */
    val filePath = "src/main/resources/data.txt" // Should be some file on your system

    /*
     * SparkSession is the entry-point for programming with the DataFrame Api.
     * Use the builder to create a spark session, local mode that can use all threads(*).
     */
    val sparkDfSession = SparkSession.builder.master("local[*]").appName("Simple Application").getOrCreate()

    /*
     * Get the SparkContext and verify settings
     */
    val sparkContext = sparkDfSession.sparkContext;
    logger.info("Is local? " + sparkContext.isLocal)
    logger.info("appName " + sparkContext.appName)

    //Can set LogLevel directly from code but all logging before this point will be verbose
    //sparkContext.setLogLevel("WARN")

    /*
     * Since sparkDf is the entrypoint for Spark DataFrame api, the read operation
     * will read in the data into a DataFrame.
     * Caching is useful for data that is to be re-used since it stores the data in memory.
     */
    val dfFile = sparkDfSession.read.textFile(filePath).cache()
    /*
     * Apply transformation filter and then initiate the computation and retrieve the result
     * with action count.
     */
    val linesWithAs = dfFile.filter(line => line.contains("a")).count()
    val linesWithBs = dfFile.filter(line => line.contains("b")).count()

    /*
     * Print results
     */
    logger.info("Number of lines containing 'a':" + linesWithAs + ", number of lines containing 'b': " + linesWithBs)

    /*
     * Stops the session and the underlying spark context
     */
    sparkDfSession.stop()
  }
}
