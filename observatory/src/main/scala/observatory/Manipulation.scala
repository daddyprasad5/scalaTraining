package observatory

//import scala.tools.nsc.backend.icode.Opcodes.opcodes.CALL_METHOD
//import scala.tools.nsc.backend.icode.Opcodes.opcodes.CALL_METHOD

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  def points(latLower: Int, latUpper:Int, lonLower: Int, lonUpper: Int): IndexedSeq[(Int, Int)] = {
    for {
      lat <- (latLower to latUpper).toIterable
      lon <- (lonLower to lonUpper).toIterable
    } yield (lat, lon)
  }

  def gridPoints(latLower: Int, latUpper:Int, lonLower: Int, lonUpper: Int, temperatures: Iterable[(Location, Double)]): collection.parallel.ParMap[(Int, Int),Double] = {
    points(latLower, latUpper, lonLower,lonUpper)
      .par.map(point =>
      (point._1, point._2)
        -> Visualization.predictTemperature(temperatures, Location(point._1, point._2)))
      .toMap
  }

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {

    val grid = gridPoints(-89,90,-180,179, temperatures)

     def getGridPoint(lat: Int, lon: Int): Double =  {
      grid((lat, lon))
    }
     getGridPoint
  }

  def ave[Key](keyVals: Iterable[(Key,Double)]): Iterable[(Key,Double)] = {
    keyVals.groupBy(_._1) //makes Map(Location -> List(Location, Double)]
      .mapValues(_.map(_._2).toList)  //makes Map(Location, List(Double))
      .map(a => (a._1, a._2.sum / a._2.length)) //Makes Map(Location, Double)
      .toIterable
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {

    val aves = ave(temperaturess.flatten)

    makeGrid(aves)

  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val knownGrid = makeGrid(temperatures)
    val pts = points(-89,90,-180,179)
    val deviations = pts.par.map(pt => ( (pt._1, pt._2), knownGrid(pt._1, pt._2) - normals(pt._1, pt._2) ) ).toMap
    (lat: Int, lon: Int) => deviations((lat, lon))
  }
}

