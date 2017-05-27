package observatory


import com.sksamuel.scrimage.Pixel
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._


@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

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

  test("great circle distance should work") {
    println("starting great circle test")
    val loc1 = Location(0.0, 180.0)
    val loc2 = Location(-12.5, 73)
    val loc3 = Location(54.6, -23.5)
    val loc4 = Location(0.0, 179.0)

    val distLoc1Loc2 = 11852
    val distLoc2Loc3 = 11555
    val distLoc1Loc4 = 111

    assert( math.abs((distLoc1Loc4) - math.abs(Visualization.greatCircleDistance(loc1, loc4).round)) < 5)
    assert( math.abs((distLoc1Loc2) - math.abs(Visualization.greatCircleDistance(loc1, loc2).round)) < 5)
    assert( math.abs((distLoc2Loc3) - math.abs(Visualization.greatCircleDistance(loc2, loc3).round)) < 5)

  }

  test("interpolation should work with 2 colors in the pallette") {
    val minColor = Color(0,0,0)
    val maxColor = Color(200,200,200)
    val minValue = 0.0
    val maxValue = 100.0
    val value = 50
    val expColor = Color(100, 100, 100)

    assert(expColor === Visualization.interpolateColor(Iterable((minValue, minColor), (maxValue, maxColor)), value))

  }

  test("interpolation should work with 3 colors in the pallette, and value being between the 1st and 2nd") {
    println("starting interpolation test1")
    val minColor = Color(0,0,0)
    val maxColor = Color(200,200,200)
    val otherColor = Color(1,1,1)
    val minValue = 0.0
    val maxValue = 100.0
    val otherValue = 1000.0
    val value = 50
    val expColor = Color(100, 100, 100)

    assert(expColor === Visualization.interpolateColor(Iterable((minValue, minColor), (maxValue, maxColor), (otherValue, otherColor)), value))

  }

  test("interpolation should work with 3 colors in the pallette, and value being between the 2nd and 3rd") {
    println("starting interpolation test2")
    val minColor = Color(0,0,0)
    val maxColor = Color(200,200,200)
    val otherColor = Color(1,1,1)
    val minValue = 50.0
    val maxValue = 100.0
    val otherValue = 0.0
    val value = 75
    val expColor = Color(100, 100, 100)

    assert(expColor === Visualization.interpolateColor(Iterable((minValue, minColor), (maxValue, maxColor), (otherValue, otherColor)), value))

  }

  test("interpolation should return lowest color with 3 colors in the pallette, and value is below the lowest value") {
    println("starting interpolation test3")
    val minColor = Color(0,0,0)
    val maxColor = Color(200,200,200)
    val otherColor = Color(1,1,1)
    val minValue = 50.0
    val maxValue = 100.0
    val otherValue = 49.0
    val value = 1
    val expColor = otherColor

    assert(expColor === Visualization.interpolateColor(Iterable((minValue, minColor), (maxValue, maxColor), (otherValue, otherColor)), value))

  }

  test("interpolation should return highest color with 3 colors in the pallette, and value is higher the highest value") {
    println("starting interpolation test4")
    val minColor = Color(0,0,0)
    val maxColor = Color(200,200,200)
    val otherColor = Color(1,1,1)
    val minValue = 50.0
    val maxValue = 100.0
    val otherValue = 49.0
    val value = 101.0
    val expColor = maxColor

    assert(expColor === Visualization.interpolateColor(Iterable((minValue, minColor), (maxValue, maxColor), (otherValue, otherColor)), value))

  }

  test("a point that is the same as a known point should have the same temperature as the known point") {
    println("starting predictTemp test 1")
    val unkLoc = Location(1.0, 1.0)
    val knownLocs = Iterable((Location(1.0, 1.0), 10.0), (Location(2.0, 2.0), 90.0), (Location(80.0, 5.0), 20.0))

    assert(Visualization.predictTemperature(knownLocs, unkLoc) === 10.0)
  }

  test("interp test from the grader") {
    val cols = List((-41995.0,Color(255,0,0)), (0.0,Color(0,0,255)))
    val value = -20997.5
    val expColor = Color(128,0,128)
    assert(expColor === Visualization.interpolateColor(cols, value))


  }

  test("interp test2 from the grader") {
    val cols = List((-3715069.0,Color(255,0,0)), (1.619798831E9,Color(0,0,255)))
    val value = 8.08041881E8
    val expColor = Color(128,0,128)
    assert(expColor === Visualization.interpolateColor(cols, value))
  }

  test("interp test3 from the grader") {
    val cols = List((1.724649806E9,Color(255,0,0)), (2.147483647E9,Color(0,0,255)))
    val value = 1.9360667265E9
    val expColor = Color(128,0,128)
    assert(expColor === Visualization.interpolateColor(cols, value))

    //List((1.724649806E9,Color(255,0,0)), (2.147483647E9,Color(0,0,255))), value = 1.9360667265E9
  }

  test("predict temperatures when all distances from known points to the unknown point are the same") {

    println("starting predict temp2")
    //define a set of known temperatures
    val knownTemps: Iterable[(Location, Double)] = {
      Iterable((Location(90, 180), 10.0), (Location(-90, -180), 0.0))
    }

    //pick an unknown point
    val location: Location = Location(0,0)

    //calculate by hand what the right prediction should be

    val expectedTemp: Int = 5
    val predictedTemp: Int = math.round(Visualization.predictTemperature(knownTemps, location)).toInt

    println("predicted temp =" + predictedTemp)

    //assert that the prediction equals the by-hand calculation
    assert(expectedTemp === predictedTemp)


  }

  test("predict temperatures when distances from 2 known points to the unknown point are different") {
    println("starting predict temp3")
    //define a set of known temperatures
    val knownTemps: Iterable[(Location, Double)] = {
      Iterable((Location(90, 180), 10.0), (Location(-45, -180), 2.0))
    }

    //pick an unknown point
    val location: Location = Location(0,0)

    //calculate by hand what the right prediction should be

    val expectedTemp: Double = BigDecimal(7.538347937004473).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    val predictedTemp: Double = BigDecimal((Visualization.predictTemperature(knownTemps, location))).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    println("predicted temp =" + predictedTemp)

    //assert that the prediction equals the by-hand calculation
    assert(expectedTemp === predictedTemp)
  }

  test("predict temperatures when distances from 3 known points to the unknown point are different") {
    println("starting predict temp3")
    //define a set of known temperatures
    val knownTemps: Iterable[(Location, Double)] = {
      Iterable((Location(90, 180), 10.0), (Location(-45, -180), 2.0), (Location(-45, 180), 5.0))
    }

    //pick an unknown point
    val location: Location = Location(0,0)

    //calculate by hand what the right prediction should be

    val expectedTemp: Double = BigDecimal(6.941068520894898).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    val predictedTemp: Double = BigDecimal((Visualization.predictTemperature(knownTemps, location))).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    println("predicted temp =" + predictedTemp)

    //assert that the prediction equals the by-hand calculation
    assert(expectedTemp === predictedTemp)
  }

 /* test("test simple image visualization") {

    println("starting image test")
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
    val temperatures = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1960, "/stations1960.csv", "/1960.csv"))

    val img = Visualization.visualize(temperatures, legend)

    val imgFile = img.output(new java.io.File("target/simpleImage.png"))

    assert(true)

  }*/

  test("super simple plot with white at top right, black at bottom left" ) {

    val stationLines = List(
      "1,1,-90,-180",  //bottom left
      "5,5,90,180"     //top right
    )

    val tempLines = List(
      "1,1,11,25,-76",   //very cold, black
      "5,5,1,29,140"      //very hot, white
    )

    val stations = Extraction.getStations(stationLines)
    val temps = Extraction.getTemps(tempLines)
    val ob = Extraction.getObservations(1960,stations, temps)
    val obs = Extraction.locationYearlyAverageRecords(ob)

    println("test getObservations output...should be -90,180,-60, 90, 180, 60")
    ob.foreach(a=> println(a._2 + " " + a._3))

    println("test yearly average output...should be -90,180,-60, 90, 180, 60")
    obs.foreach(a=> println(a._1 + " " + a._2))

    val img = Visualization.plotter(obs, legend, 90, 180)

    //mark the center, top right and top left so I know I'm correctly testing the right pixels
/*    img.setPixel(180,90, Pixel(255,0,0, 255))
    img.setPixel(0,180, Pixel(255,0,0, 255))
    img.setPixel(360,0, Pixel(255,0,0, 255))*/

    val imgFile = img.output(new java.io.File("target/supersimpleplot.png"))

    println(img.center)

/*    val colorsBottomLeftToTopRight = for {
      x <- 0 to 8
      y <- 0 to 4
    } yield (x,y,img.argb(x,y))

    colorsBottomLeftToTopRight.foreach(a => {
      println(a._1 + ", " + a._2 + ": " + a._3(1) + ", " + a._3(2) + ", " + a._3(3))
    })*/

    println(img.argb(180,90)(1))
    println(img.argb(180,90)(2))
    println(img.argb(180,90)(3))
    println("")

    println(img.argb(0,90)(1))
    println(img.argb(0,90)(2))
    println(img.argb(0,90)(3))
    println("")

    println(img.argb(360,0)(1))
    println(img.argb(360,0)(2))
    println(img.argb(360,0)(3))
    println("")

    //center should be blue
    assert(img.argb(180,90)(1) === 0)
    assert(img.argb(180,90)(2) === 255)
    assert(img.argb(180,90)(3) === 255)

    //bottom left should be black
    assert(img.argb(0,180)(1) === 0)
   // assert(img.argb(0,180)(2) === 0)
    assert(img.argb(0,180)(3) === 0)

    //top right should be white
    assert(img.argb(360,0)(1) === 255)
    assert(img.argb(360,0)(2) === 255)
    assert(img.argb(360,0)(3) === 255)

  }


  test("output the image") {
    println("starting image test")
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


    val temperatures = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1977, "/stations.csv", "/1977.csv"))

    val img = Visualization.visualize(temperatures, legend)

    val imgFile = img.output(new java.io.File("target/1977image.png"))


    assert(true)
  }
}
