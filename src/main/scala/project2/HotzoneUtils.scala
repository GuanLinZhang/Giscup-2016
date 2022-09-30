package project2

object HotzoneUtils {

  def ST_Contains(queryRectangle: String, pointString: String ): Boolean = {
    // YOU NEED TO CHANGE THIS PART
    // return true // YOU NEED TO CHANGE THIS PART
    val rectPoints = queryRectangle.split(",")
    val firstX = rectPoints(0).trim().toDouble
    val firstY = rectPoints(1).trim().toDouble
    val secondX = rectPoints(2).trim().toDouble
    val secondY = rectPoints(3).trim().toDouble

    val point = pointString.split(",")
    val pointX = point(0).trim().toDouble
    val pointY = point(1).trim().toDouble

    val largeX = math.max(firstX, secondX)
    val largeY = math.max(firstY, secondY)
    val smallX = math.min(firstX, secondX)
    val smallY = math.min(firstY, secondY)

    if (pointX >= smallX && pointY >= smallY && pointX <= largeX && pointY <= largeY) 
    {
      return true
    } 
    else 
    {
      return false
    }
  }

  // YOU NEED TO CHANGE THIS PART

}
