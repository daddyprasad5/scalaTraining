package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.annotation.tailrec
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  case class Tile(x: Int,y: Int, z: Short){
    def toLatLon =  new LatLonPoint(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1<<z))))),
      x.toDouble / (1<<z) * 360.0 - 180.0, z)
    def toURI = new java.net.URI("http://tile.openstreetmap.org/"+z+"/"+x+"/"+y+".png")
  }

  case class LatLonPoint(lat: Double, lon: Double, z: Short){
    def toTile = new Tile(
      ((lon + 180.0) / 360.0 * (1<<z)).toInt,
      ((1 - log(tan(toRadians(lat)) + 1 / cos(toRadians(lat))) / Pi) / 2.0 * (1<<z)).toInt,
      z)
  }

  /**
    * @param x X coordinate
    * @param y Y coordinate
    * @return a sequence of x,y pairs for the four sectors at the next highest zoom level
    * */
  def subTiles(x: Int, y: Int): Seq[(Int, Int)] = {
    Seq(
      (2*x,2*y),
      (2*x+1,2*y),
      (2*x,2*y+1),
      (2*x+1,2*y+1)
      )
  }

  /**
    * @param x X coordinate
    * @param y Y coordinate
    * @param zoom the number of incremental zooms.  each zoom will expand each pixel into 4 pixels
    * @return a sequence of x,y pairs for the four sectors at the next highest zoom level
    * */
  def subTilesPower(x: Int, y: Int, zoom: Int): Seq[(Int, Int)] = {
    @tailrec def iter(coords: Seq[(Int,Int)],zoom: Int): Seq[(Int, Int)] = {
      if (zoom == 0) coords
      else {
        val nextCoords = coords.flatMap(a => subTiles(a._1, a._2))
        iter(nextCoords, zoom - 1)
      }
    }
    iter(subTiles(x,y), zoom-1).sortBy(a => (a._2, a._1))  //is this the right order
  }

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {

    val lon = ((x.toDouble / (1<<zoom)) * 360.0) - 180.0
    val lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1<<zoom)))))
    Location(lat, lon)

  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val subtiles = subTilesPower(x,y,8)
    println("subtiles.size " + subtiles.size)
    val locations = subtiles.par.map(t => tileLocation(zoom+8, t._1, t._2))
    println("locations.size " + locations.size)
    val temps = locations.par.map(l=> Visualization.predictTemperature(temperatures, l))
    println("temps.size " + temps.size)
    val lColors = temps.par.map(t => Visualization.interpolateColor(colors, t))
    println("lColors.size " + lColors.size)
    val pixels = lColors.par.map(c => Pixel(c.red, c.green, c.blue, 127))
    println("pixels size is " + pixels.size)
    Image(256,256,pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    val zooms = 0 to 3
    val xys: Map[Int, (Iterable[Int], Iterable[Int])] = {
      Map(
        (0, ( Iterable(0), Iterable(0) ) ),
        (1, ((0 to 1).toIterable, (0 to 1).toIterable)),
        (2, ((0 to 3).toIterable, (0 to 3).toIterable)),
        (3, ((0 to 7).toIterable, (0 to 7).toIterable))
      )
    }

    yearlyData.foreach(yd => {
      zooms.foreach(z => {
        xys(z)._1.foreach(x => {
          xys(z)._2.foreach(y => {
            generateImage(yd._1, z, x, y, yd._2)
          })
        })
      })
    })
  }
}
