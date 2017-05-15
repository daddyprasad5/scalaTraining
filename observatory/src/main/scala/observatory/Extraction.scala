package observatory

import java.time.LocalDate
import scala.io.Source
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.sql._
import org.apache.spark.rdd._
import org.apache.spark.sql.types._

import org.apache.log4j.{Level, Logger}



/**
  * 1st milestone: data extraction
  */
case class Station(stn: Integer, wban: Option[Integer], lat: Double, lon: Double) {}
case class TempReading(stn: Integer, wban: Option[Integer], month: Int, day: Int, temp: Double) {}
case class FinalTempReading(date: LocalDate, location: Location, temp: Double)

object Extraction {


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    println("starting locateTemperatures")

    val fileStream = getClass.getResourceAsStream(stationsFile)
    val lines = Source.fromInputStream(fileStream).getLines.toList
    val allStations = lines.map(line => line.split(",", -1))
    val stations: Seq[Station] = for (
      arr <- allStations
      //if !(arr(0) == "")
      if !(arr(2) == "")
      if !(arr(3) == ""))yield {
       Station(
        stn = if (arr(0) == "") null else arr(0).toInt,
        wban = if (arr(1) == "") None else Some(arr(1).toInt),
        lat = arr(2).toDouble,
        lon = arr(3).toDouble)
    }

    val tempFileStream = getClass.getResourceAsStream(temperaturesFile)
    val tempLines = Source.fromInputStream(tempFileStream).getLines.toList
    val allTemps = tempLines.map(line => line.split(",", -1))
    val temps  = for (
      arr <- allTemps
      //if !(arr(0) == "")
      if !(arr(2) == "")
      if !(arr(3) == "")
      if !(arr(4) == "")) yield {
      TempReading(
        stn = if (arr(0) == "") null else arr(0).toInt,
        wban = if (arr(1) == "") None else Some(arr(1).toInt),
        month = arr(2).toInt,
        day = arr(3).toInt,
        temp = ((arr(4).toDouble - 32) * 5) / 9 //celsius = ((fahrenheit - 32)*5)/9;
      )
    }

    println("temps created")

    val conf: SparkConf = new SparkConf().setMaster("local").setAppName("My app")
    val sc: SparkContext = new SparkContext(conf)
    val spark = SparkSession
      .builder()
      .appName("MySparkApp")
      .config("spark.default.parallelism", "8")
      .getOrCreate()

    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

    import spark.implicits._

    val stationsDF = sc.parallelize(stations).toDF()
    sc.broadcast(stationsDF)

    //println(stationsDF.rdd.partitions.size)

    //println("stations")
    //stationsDF.collect().foreach(println(_))

    val tempsDF = sc.parallelize(temps).toDF()

    //println(tempsDF.rdd.partitions.size)

    //println("temps")
    //tempsDF.collect().foreach(println(_))

    val observations = tempsDF
      .join(stationsDF,
        tempsDF("stn") <=> stationsDF("stn") &&
        tempsDF("wban") <=> stationsDF("wban"))
      .select("month", "day", "lat", "lon", "temp")

    //observations.collect().foreach(println(_))

    val obs_out = observations.rdd.map(row => {
      (LocalDate.of(year, row.getInt(0), row.getInt(1)), Location(row.getDouble(2), row.getDouble(3)), row.getDouble(4))
    }).collect()

    sc.stop()

    obs_out.toSeq

    /*Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )*/
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    println("starting locationYearlyAverageRecords")

    val justTemps = records.map(rec => {
      (rec._2, rec._3)
    }).toSeq

    val conf: SparkConf = new SparkConf().setMaster("local").setAppName("My app")
    val sc: SparkContext = new SparkContext(conf)
    val spark = SparkSession
      .builder()
      .appName("MySparkApp")
      .config("spark.default.parallelism", "8")
      .getOrCreate()

    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

    val locYrlyAveRec = sc.parallelize(justTemps).aggregateByKey((0.0, 0.0))(
      (acc, value) => (acc._1 + value, acc._2 + 1),
      (acc1, acc2) => (acc1._1 + acc2._1, acc1._2 + acc2._2))
      .mapValues(sumCount => 1.0 * sumCount._1 / sumCount._2)
      .collect

    sc.stop()

    locYrlyAveRec

  }

}
