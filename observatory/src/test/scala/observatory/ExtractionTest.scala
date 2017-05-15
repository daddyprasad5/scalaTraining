package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import java.time.LocalDate
import scala.io.Source



@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("locate temperatures should work...") {
    val corrSeq = Set(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.299999999999997),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.000000000000001)
    )

    val testSeq = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")

    assert(corrSeq == testSeq.toSet)

  }

  test("average temps should work...") {

    val corrSeq = Set(
      (Location(37.35, -78.433), 27.299999999999997),
      (Location(37.358, -78.438), 1.0000000000000004)
    )

    val testSeq = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv"))

    testSeq.foreach(x => println(x))

    assert(corrSeq == testSeq.toSet)

  }


}