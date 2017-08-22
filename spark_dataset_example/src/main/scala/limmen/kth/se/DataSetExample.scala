package limmen.kth.se

import org.apache.log4j.{Level, LogManager, Logger}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.to_timestamp
import org.apache.spark.sql.types.DataTypes

/*
 * Dataset unifies the untyped Relational/Table view of Dataframe and the
 * Typed functional RDD view into a strongly typed API.
 * We can make a relational DataFrame into a Dataset by providing
 * a schema. Spark can create encoders automatically from case classes.
 * This must be defined out of the object scope for it to work.
 */
case class Participant(id: Int, name: String, gender: String, country: String, birth: java.sql.Timestamp, height: Double, weight: Double, sport: String)

case class Person(name: String, age: Long)

/*
 * Dataset example, trying out the Dataset API and sparkSQL
  * Run intructions:
  * 1. package jar with 'sbt package'
  * 2. Submit jar to cluster or local spark: /home/limmen/programs/spark-2.2.0-bin-hadoop2.7/bin/spark-submit --class "limmen.kth.se.DataSetExample" target/scala-2.11/spark_dataset_example_2.11-0.1.0-SNAPSHOT.jar
  *
  * Alternatively you can simply run from within sbt with 'sbt run' but then you have to live with "InterruptedException"
 */
object DataSetExample {

  val logger = LogManager.getRootLogger

  def main(args: Array[String]) = {

    Logger.getLogger("org").setLevel(Level.ERROR)
    Logger.getLogger("akka").setLevel(Level.ERROR)
    val filePath = "src/main/resources/participants.tsv"
    val sparkSession = SparkSession.builder.master("local[*]").appName("NOG-Clustering").getOrCreate()

    // For implicit conversions like converting RDDs to DataFrames
    import sparkSession.implicits._

    /*
     * Dataset[Row] is equivalent to a DataFrame.
     * Here we create a DataFrame with named columns
     */
    val rawData = sparkSession.read.option("sep", "\t").option("header", true).csv(filePath).cache().toDF("id", "name", "gender", "country", "birth", "height", "weight", "sport")

    /*
     * withColumn is a method that returns a new Dataset by adding a column or
     * replacing the existing column that has the same name.
     * $ is a valid method in scala and gets the "value of a param in the embedded param map or its default value"
     */
    val parsedData = rawData.withColumn("birth", to_timestamp($"birth", "YYYY/MM/dd"))

    /*
     * When we create Dataset with out schema Spark will complain
     * about truncation for converting scertain fields so we must
     * cast those fields excplicitly first to avoid this.
     */
    val parsedData2 = parsedData.withColumn("id", $"id".cast(DataTypes.IntegerType)).withColumn("height", $"height".cast(DataTypes.DoubleType)).withColumn("weight", $"weight".cast(DataTypes.DoubleType))

    //val schema = Encoders.product[Participant].schema

    /*
     * Convert the relational Dataframe into strongly typed relational dataset
     */
    val typedDataSet = parsedData2.as[Participant]

    /*
     * A DataFrame is a Dataset organized into named columns.
     * It is conceptually equivalent to a table in a relational
     * database or a data frame in R/Python, but with richer
     * optimizations under the hood. DataFrames can be constructed
     * from a wide array of sources such as: structured data files,
     * tables in Hive, external databases, or existing RDDs.
     * Dataset provides the benefits of RDDs (strong typing,
     * ability to use powerful lambda functions) with the
     * benefits of Spark SQL’s optimized execution engine.
     */

    typedDataSet.printSchema()

    /*
     * By naming the virtual table that DF/DS represents we can issue SQL queries against it
     */

    typedDataSet.createOrReplaceTempView("participants")

    logger.info("SELECT * FROM participants:")
    sparkSession.sql("SELECT * FROM participants").show()

    logger.info("SELECT * FROM participants WHERE name='Salminen, Lena'")
    sparkSession.sql("SELECT * FROM participants WHERE name='Salminen, Lena'").show()

    logger.info("SELECT * FROM participants WHERE weight > 70")
    sparkSession.sql("SELECT * FROM participants WHERE weight > 70").show()

    /*
     * A Dataset can be constructed from JVM objects and then
     * manipulated using functional transformations (map, flatMap, filter, etc.).
     * Encoders are created for case classes
     * Encoders for most common types are automatically provided by importing spark.implicits._
     */
    val caseClassDS = Seq(Person("Andy", 32)).toDS()
    caseClassDS.show()

    sparkSession.stop()
  }
}
