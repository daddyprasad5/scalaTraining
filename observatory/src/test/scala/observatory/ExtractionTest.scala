package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import java.time.LocalDate



@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("getStations should work") {
    val lines = List(
      "1,1,-2,4",
      "2,2,2,4",
      "3,3,-2,4",
      "4,4,-2,-4",
      "5,5,0,0"
    )

    val lat0 = -2.0
    val lat1 = 2.0
    val lon2 = 4.0
    val lon3 = -4.0
    val lat4 = 0.0

    val stations = Extraction.getStations(lines).toArray

    assert(lat0 === stations(0).lat)
    assert(lat1 === stations(1).lat)
    assert(lon2 === stations(2).lon)
    assert(lon3 === stations(3).lon)
    assert(lat4 === stations(4).lat)
  }

  test("fahrenheitToCelsius should work") {
    val f1 = 140
    val f2 = 41.18
    val f3 = -76
    val c1 = 60
    val c2 = 5
    val c3 = -60

    assert(Extraction.fahrenheitToCelsius(f1).round === c1)
    assert(Extraction.fahrenheitToCelsius(f2).round === c2)
    assert(Extraction.fahrenheitToCelsius(f3).round === c3)
  }

  test("getTemps should work") {
    val lines = List(
      "10013,,11,25,39.2",
      "724017,,8,11,81.14",
      "724017,3707,12,6,32",
      "724017,3707,1,29,35.6"
    )

    val temp0 = Extraction.fahrenheitToCelsius(39.2)
    val month1 = 8
    val day2 = 6
    val wban2 = 3707
    val stn3 = 724017

    //stn: Integer, wban: Option[Integer], month: Int, day: Int, temp: Double

    val temps = Extraction.getTemps(lines).toArray

    assert(temp0 === temps(0).temp)
    assert(month1 === temps(1).month)
    assert(day2 === temps(2).day)
    assert(temps(1).wban.isEmpty)
    assert(wban2 === temps(2).wban.get)
    assert(stn3 == temps(3).stn)

  }

  test("get observations should work") {

    case class Obs(date: LocalDate, location: Location, temp: Double)

    val stationLines = List(
      "1,1,-1,-1",
      "2,2,2,2",
      "3,3,3,4",
      "4,4,4,4",
      "5,5,5,5"
    )

    val tempLines = List(
      "1,1,11,25,39.2",
      "2,2,8,11,81.14",
      "3,3,12,6,32",
      "4,4,1,29,35.6"
    )

    val obs = Extraction.getObservations(1960, Extraction.getStations(stationLines), Extraction.getTemps(tempLines))
    val obsCC = (for (o <- obs) yield Obs(o._1, o._2, o._3)).toArray
    val month0 = 11
    val day1 = 11
    val lat2 = 3
    val lon3 = 4
    val temp1 = Extraction.fahrenheitToCelsius(81.14)

    obsCC.foreach(a => println(a.date.getMonthValue))

    assert(month0 === obsCC(0).date.getMonthValue)
    assert(day1 === obsCC(1).date.getDayOfMonth)
    assert(lat2 === obsCC(2).location.lat)
    assert(lon3 === obsCC(3).location.lon)
    assert(temp1 === obsCC(1).temp)

  }

  test("locate temperatures should work...") {
    val corrSeq = Set(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.299999999999997),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.000000000000001)
    )

    val testSeq = Extraction.locateTemperatures(2015, "/stationsExtractTest.csv", "/2015ExtractionTest.csv")

    assert(corrSeq == testSeq.toSet)


  }

  test("average temps should work...") {

    val corrSeq = Set(
      (Location(37.35, -78.433), 27.299999999999997),
      (Location(37.358, -78.438), 1.0000000000000004)
    )

    val testSeq = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2015, "/stationsExtractTest.csv", "/2015ExtractionTest.csv"))

    testSeq.foreach(x => println(x))

    assert(corrSeq == testSeq.toSet)

  }

  test("locationyearlyaverage should work...") {

    val obs: Iterable[(LocalDate, Location, Double)] =
      Iterable (
        (LocalDate.of(1960, 1,1), Location(1,1), 0.0),
        (LocalDate.of(1960, 1,1), Location(1,1), 10.0),
        (LocalDate.of(1960, 1,1), Location(2,-2), -5.0),
        (LocalDate.of(1960, 1,1), Location(2,-2), -2.0),
        (LocalDate.of(1960, 1,1), Location(-1,-1), -1.0)
      )

    val ave11 = 5.0
    val ave22 = -3.5
    val ave11neg = -1.0

    val aves = Extraction.locationYearlyAverageRecords(obs).toMap

    assert(ave11 === aves(Location(1,1)))
    assert(ave22 === aves(Location(2,-2)))
    assert(ave11neg === aves(Location(-1,-1)))

  }


}