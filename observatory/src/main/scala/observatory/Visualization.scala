package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.spark.sql.{Encoders, SparkSession, functions}
import org.apache.spark.{SparkContext, SparkConf}
import scala.math.acos
import scala.math.sin
import scala.math.cos
import scala.math.abs


/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    //println("predict temperature for " + location)
    /**
      *
      * @param loc1 Location of a first point on a sphere
      * @param loc2 Location of a second point on a sphere
      * @return The arc length (in km) between the two points, traveling over the sphere's surface
      */

    def greatCircleDistance (loc1: Location, loc2: Location): Double = {


      val radius = 6371 // radius of the earth in km
      val lat1 = loc1.lat
      val lon1 = loc1.lon
      val lat2 = loc2.lat
      val lon2 = loc2.lon

      val dLat = toRadians(lat2-lat1)
      val dLon = toRadians(lon2-lon1)

      val a = Math.sin(dLat/2) * Math.sin(dLat/2) +
        Math.cos(toRadians(lat1)) *   Math.cos(toRadians(lat2)) *
          Math.sin(dLon/2) * Math.sin(dLon/2)

      val c = 2 * Math.asin(Math.sqrt(a))

      val dist = radius * c

      //println("distance " + dist)
      dist
    }

    def toRadians (degree: Double): Double = {
        // Value degree * Pi/180
        val res = degree * 3.1415926 / 180
        res
    }

    //compute all the distances to known points
    val tempDF = temperatures//sc.parallelize(temperatures.toSeq)
    val distances = tempDF.map(_ match {
          case (l: Location, t: Double) => {
            //println("l, t, location is " + l + t + location)
            (l, t, greatCircleDistance(l, location))
          }
    })


    val minDist = distances.reduce((a,b)=> if (a._3 < b._3) a else b)

    //if the closest known point is less than 1 km, use the temperature of that closest known point as the solution
    //otherwise use inverse distance weighting to calculate the temperature

    if (minDist._3 <= 1) minDist._2
    else {
      val p = 2  //experiment with this value...
      val weightedDistAndTemp = distances.map( x => (1/scala.math.pow(x._3,p), (1/scala.math.pow(x._3,p)) * x._2))
      val summedDistAndTemp = weightedDistAndTemp.reduce( (a,b) => (a._1 + b._1, a._2 + b._2))
      summedDistAndTemp._2 / summedDistAndTemp._1
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    //println("interp color for " + value)
    def findBounds(points: Iterable[(Double, Color)], value: Double, lower: Option[(Double, Color)]): (Option[(Double, Color)], Option[(Double, Color)]) = {
      if (points.isEmpty) (lower, None)
      else if (value < points.head._1) (lower, Some(points.head))
      else findBounds(points.tail, value, Some(points.head))
    }

    def interp(min: (Double, Double), max: (Double, Double), x: Double): Int = {
      //println("min: " + min)
      //println("max: " + max)
      //println("x: " + x)
      val m = (max._2 - min._2) / (max._1 - min._1)
      //println("m: "+ m)
      val b = max._2 - (m*max._1)
      //println("b: "+ b)
      val y = ((m*x) + b)
      //println("y: " + y)
      if (y < 0 ) 0 else (y+.5000000005).toInt
    }

    val sortedPoints = points.toSeq.sortWith((a, b) => a._1 < b._1)

    val bounds = findBounds(sortedPoints, value, None)

    //handle lower than min and higher than max colors
    if (bounds._1.isEmpty) bounds._2.get._2
    else if (bounds._2.isEmpty) bounds._1.get._2
    else {
      val red = interp((bounds._1.get._1, bounds._1.get._2.red), (bounds._2.get._1, bounds._2.get._2.red), value)
      val blue = interp((bounds._1.get._1, bounds._1.get._2.blue), (bounds._2.get._1, bounds._2.get._2.blue), value)
      val green = interp((bounds._1.get._1, bounds._1.get._2.green), (bounds._2.get._1, bounds._2.get._2.green), value)

      Color(red, green, blue)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    println("starting visualize")

    val conf: SparkConf = new SparkConf().setMaster("local").setAppName("My app")
    val sc: SparkContext = new SparkContext(conf)
    val spark = SparkSession
      .builder()
      .appName("MySparkApp")
      .config("spark.default.parallelism", "8")
      .getOrCreate()

    def lonToPix (lon: Double): Int = lon.round.toInt + 180
    def latToPix (lat: Double): Int = lat.round.toInt + 90

    //create a seq of all (lon, lat) integer coordinates
    val coord: Seq[(Location)] = for {
      lon <- -180 to 180
      lat <- -90 to 90
      //lon <- -50 to 50
      //lat <- -50 to 50

    } yield Location(lat, lon)


    //parallelize the sequences
    val coordRDD = sc.parallelize(coord)
    println("finished parallelizing coordRDD")

    sc.broadcast(temperatures)
    sc.broadcast(colors)

    //get the temp and then color of each coordinate
    val pixels = coordRDD
      .map(loc => (loc, predictTemperature(temperatures, loc)))
      .map(locTemp => interpolateColor(colors, locTemp._2))
      .map(c => Pixel(c.red, c.green, c.blue, 255))
      .collect()

    sc.stop()

    println("finished creating pixels")

    //val pxPrint = pixels.take(10).map(a => (a.red, a.green, a.blue, a.alpha))
    //pxPrint.foreach(a => println("red: " + a._1 + "green:" + a._2+ "blue:" + a._3 + "alpha:" + a._4))

    Image(361, 181, pixels)
    //Image(101, 101, pixels)

  }
}

