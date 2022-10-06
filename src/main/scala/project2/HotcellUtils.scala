package project2

import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Calendar

object HotcellUtils {
  val coordinateStep = 0.01

  def CalculateCoordinate(inputString: String, coordinateOffset: Int): Int = {
    // Configuration variable:
    // Coordinate step is the size of each cell on x and y
    var result = 0
    coordinateOffset match {
      case 0 =>
        result = Math
          .floor(
            (inputString
              .split(",")(0)
              .replace("(", "")
              .toDouble / coordinateStep)
          )
          .toInt
      case 1 =>
        result = Math
          .floor(
            inputString.split(",")(1).replace(")", "").toDouble / coordinateStep
          )
          .toInt
      // We only consider the data from 2009 to 2012 inclusively, 4 years in total. Week 0 Day 0 is 2009-01-01
      case 2 => {
        val timestamp = HotcellUtils.timestampParser(inputString)
        result =
          HotcellUtils.dayOfMonth(timestamp) // Assume every month has 31 days
      }
    }
    return result
  }

  def timestampParser(timestampString: String): Timestamp = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
    val parsedDate = dateFormat.parse(timestampString)
    val timeStamp = new Timestamp(parsedDate.getTime)
    return timeStamp
  }

  def dayOfYear(timestamp: Timestamp): Int = {
    val calendar = Calendar.getInstance
    calendar.setTimeInMillis(timestamp.getTime)
    return calendar.get(Calendar.DAY_OF_YEAR)
  }

  def dayOfMonth(timestamp: Timestamp): Int = {
    val calendar = Calendar.getInstance
    calendar.setTimeInMillis(timestamp.getTime)
    return calendar.get(Calendar.DAY_OF_MONTH)
  }

  // YOU NEED TO CHANGE THIS PART
  /** Check whether two points are neighbor
    */
  def ST_Within(p1: Point, p2: Point): Boolean = {
    val distance = 1 // the coordinate of cells has been normalized
    (math.abs(p1.x - p2.x) <= distance) && (math.abs(
      p1.y - p2.y
    ) <= distance) &&
    (math.abs(p1.z - p2.z) <= distance)
  }

  def ConvertInputPoint(inputString: String, coordinateOffset: Int): Double = {
    var result = 0.0
    coordinateOffset match {
      case 0 =>
        result =
          inputString.split(",")(0).replace("(", "").toDouble / coordinateStep
      case 1 =>
        result =
          inputString.split(",")(1).replace(")", "").toDouble / coordinateStep
      case 2 =>
        // We only consider the data from 2009 to 2012 inclusively, 4 years in total. Week 0 Day 0 is 2009-01-01
        val timestamp = HotcellUtils.timestampParser(inputString)
        result =
          HotcellUtils.dayOfMonth(timestamp) // Assume every month has 31 days
    }
    result
  }

  /** My custom Point object
    */
  case class Point(x: Double, y: Double, z: Double)

  def parsePointStr(pointStr: String): Point = {
    val p1 = pointStr.split(":") map { case (x) => x.toDouble }
    Point(p1(0), p1(1), p1(2))
  }

}
