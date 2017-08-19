package limmen.kth.se

import org.apache.log4j.{ Level, LogManager, Logger }
import org.apache.spark.ml.Pipeline
import org.apache.spark.ml.clustering.{KMeans, KMeansModel}
import org.apache.spark.ml.feature.VectorAssembler
import org.apache.spark.sql._;
import org.apache.spark.sql.types.DataTypes

/**
  * Clustering of the NOG dataset
  *
  * Run intructions:
  * 1. package jar with 'sbt package'
  * 2. Submit jar to cluster or local spark: /home/limmen/programs/spark-2.2.0-bin-hadoop2.7/bin/spark-submit --class "limmen.kth.se.NOG" target/scala-2.11/spark_kmeans_nog_2.11-0.1.0-SNAPSHOT.jar
  *
  * Alternatively you can simply run from within sbt with 'sbt run' but then you have to live with "InterruptedException"
  */
object NOG {

  val logger = LogManager.getRootLogger
  def main(args: Array[String]) = {

    /*
     * Turn down verbosity of spark-logging programatically through Log4j API
     */
    Logger.getLogger("org").setLevel(Level.ERROR)
    Logger.getLogger("akka").setLevel(Level.ERROR)

    /*
     * Specify data file
     */
    val filePath = "src/main/resources/participants.tsv"

    /*
     * SparkSession is the entry-point for programming with the DataFrame Api.
     * Use the builder to create a spark session, local mode that can use all threads(*).
     */
    val sparkDfSession = SparkSession.builder.master("local[*]").appName("NOG-Clustering").getOrCreate()

    /*
     * Load data in format of TSV file into a DataFrame, cache it for re-use
     * Header means that first line is interpreted as column-labels.
     */
    val dfFile = sparkDfSession.read.option("sep", "\t").option("header", true).csv(filePath).cache()

    /*
     * Convert column types to Integers since that is required for the KMeans model.
     * Finally fill null-values with 0s.
     */
    val parsedDf = dfFile.withColumn("id", dfFile.col("id").cast(DataTypes.IntegerType)).withColumn("height", dfFile.col("height").cast(DataTypes.IntegerType)).withColumn("weight", dfFile.col("weight").cast(DataTypes.IntegerType)).withColumn("name", dfFile.col("name").cast(DataTypes.IntegerType)).withColumn("gender", dfFile.col("gender").cast(DataTypes.IntegerType)).withColumn("country", dfFile.col("country").cast(DataTypes.IntegerType)).withColumn("birth", dfFile.col("birth").cast(DataTypes.IntegerType)).withColumn("sport", dfFile.col("sport").cast(DataTypes.IntegerType)).na.fill(0)

    /*
     * Show top 20 rows of parsed table
     */
    logger.info("Parsed Table: ")
    parsedDf.show()

    /*
     * Assembler is a utility function that combines given list of columns into a single vector column "features"
     */
    val assembler = new VectorAssembler().setInputCols(Array("name", "gender", "country", "birth", "height", "weight", "sport")).setOutputCol("features")

    /*
     * Parameters for the model, K is the number of clusters and is a bias.
     * clusterSeed is used to initialize points for the clusters
     */
    val K = 5
    val clusterSeed = 1L
    val maxIterations = 1000

    /*
     * Create KMeans model
     */
    val kmeans = new KMeans().setK(K).setSeed(clusterSeed).setMaxIter(maxIterations)

    /*
     * Creating pipeline, defines the workflow for this ML program.
     * In this case we have 1 transformer (assembler which implements transform()) that creates a new transformed DF
     * And 1 Estimator (kmeans which implements fit()) and produces a model.
     *
     * Why pipelines?
     * In machine learning, it is common to run a sequence of algorithms to process and learn from data.
     *  E.g., a simple text document processing workflow might include several stages:
     * - Split each document’s text into words.
     * - Convert each document’s words into a numerical feature vector.
     * - Learn a prediction model using the feature vectors and labels.
     */
    val pipeline = new Pipeline().setStages(Array(assembler, kmeans))

    /*
     * Train the model
     */
    val pipelineModel = pipeline.fit(parsedDf)
    /*
     * Extract kMeansModel from the pipelineModel
     */
    val kMeansModel = pipelineModel.stages(1).asInstanceOf[KMeansModel]

    /*
     * Evaluate performance of the model.
     * computeCost() returns the K-means cost, i.e sum of squared distances of
     * points to their nearest center for this model on the given data
     */
    val sumOfSquaredErrors = kMeansModel.computeCost(assembler.transform(parsedDf))

    /*
     * Print model results
     */
    logger.info("SumOfSquaredErrors of the model on the training-set: " + sumOfSquaredErrors)
    logger.info("Cluster centers:")
    kMeansModel.clusterCenters.foreach(center => logger.info("center: " + center))

    /*
     * Stops the session and the underlying spark context
     */
    sparkDfSession.stop()
  }
}
