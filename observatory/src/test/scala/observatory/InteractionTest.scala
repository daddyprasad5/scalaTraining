package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.concurrent.TrieMap



@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  val legend: Iterable[(Double, Color)] = {
    Iterable(
      (60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0))
    )
  }

 /* test("tileLocation should work") {

    println("tileLocation should work")

    //create a mutable map of locations to pixels
    val coordImmut = (for {
      lati <- (-90 to 90).map(_ * -1)
      long <- -180 to 180
    } yield (Location(lati, long), Pixel(255,255,255,255))).toMap

    println(coordImmut.take(10))

    //make the map mutable
    val coord = collection.mutable.Map() ++ coordImmut

    //create (x,y) coordinates for each of 16 sectors in a zoom=2 grid
    val topLeftXYs = for{
      x <- 0 to 3
      y <- 0 to 3
    } yield (x,y)

    println("print top left coord of each sector")
    topLeftXYs.foreach(println(_))

    println("print the lat, lon of top left of each sector")
    topLeftXYs.foreach(a => println(Interaction.tileLocation(2, a._1, a._2)))

/*    println("print the pixels at each coordinate location")
    topLeftXYs.foreach(a => {
      println(coord(Interaction.tileLocation(2,a._1, a._2)))
    })

    topLeftXYs.foreach(a => {
      coord(Interaction.tileLocation(2,a._1, a._2))  = Pixel(0,0,0,255)
    })*/

/*    val pixels = (for {
      lati <- (-90 to 90).map(_ * -1)
      long <- -180 to 180
    } yield (coord(Location(lati, long)))).toArray

    val img = Image(361, 181, pixels)

    val imgFile = img.output(new java.io.File("target/tileLocationTestImage.png"))*/

  }

  test("a small tile with one known color should be all that color") {
    val knownTemps: Iterable[(Location, Double)] = {
      Iterable((Location(0, 0), -100.0))
    }

    val img = Interaction.tile(knownTemps, legend, 0, 0, 0)

    //img.foreach((x,y,p) => println(x + "," + y + " " + p.red + "," + p.green + "," + p.blue))

    img.foreach((x,y,p) => {
      assert(p.red === 0)
      assert(p.green === 0)
      assert(p.blue ===0)
    })

    val imgFile = img.output(new java.io.File("target/tileLocationTestImage2.png"))

  }

  test("lets' take a look at the tiles subTilesPower function") {
    //try to figure out why the map is doubled east to west, and lots of emptiness in the bottom half

    val test = Interaction.subTilesPower(0,0,2): Seq[(Int, Int)]
    val expected =
      Seq (
        (0,0),
        (1,0),
        (2,0),
        (3,0),
        (0,1),
        (1,1),
        (2,1),
        (3,1),
        (0,2),
        (1,2),
        (2,2),
        (3,2),
        (0,3),
        (1,3),
        (2,3),
        (3,3)
      )
    assert( test === expected )
  }

  test("let's take a look at the tileToLonLat with zoom = 1") {

    println("zoom = 1")
    val coords =
      Seq (
        (0,0),
        (1,0),
        (0,1),
        (1,1)
      )


    val test = coords.map(a => Interaction.tileLocation(a._1, a._2, 1))

    test.foreach(println(_))

  }

  test("let's take a look at the tileToLonLat with power = 2") {

    println("zoom = 2")
    val coords = Interaction.subTilesPower(0,0,2): Seq[(Int, Int)]

    coords.foreach(println(_))

    val test = coords.map(a => Interaction.tileLocation(a._1, a._2, 2))

    test.foreach(println(_))

  }*/

 /*  test("zero zoom image should look like the unzoomed world") {

   val temperatures = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1977, "/stations.csv", "/1977.csv"))

   //val straigh1977img = Visualization.visualize(temperatures, legend)

   //val straigh1977imgFile = straigh1977img.output(new java.io.File("target/1977image.png"))

     //temps, colors, zoom, x, y
   val tile1977ZeroZoomimg = Interaction.tile(temperatures, legend, 2, 0, 1)

   val tile1977ZeroZoomFile = tile1977ZeroZoomimg.output(new java.io.File("target/tile1977ZeroZoomimg.png"))

  }*/

  test("generateData should work") {
    //generateTiles[Data](yearlyData: Iterable[(Int, Data)],generateImage: (Int, Int, Int, Int, Data) => Unit)

    def testGenerate(year: Int, zoom: Int, x: Int, y: Int, data: Int): Unit = {
      val URL = "target/temperatures/" + year + "/" + zoom + "/" + x + "-" + y + ".png"
      println(URL)
      //println(data)
    }

    val testData: Iterable[(Int, Int)] =
      Iterable(
        (1966, 1)/*,
        (1967, 2)*/
      )

    Interaction.generateTiles(testData, testGenerate)

  }


}
