package project2

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions.udf
import org.apache.spark.sql.functions._

object HotcellAnalysis {
  Logger.getLogger("org.spark_project").setLevel(Level.WARN)
  Logger.getLogger("org.apache").setLevel(Level.WARN)
  Logger.getLogger("akka").setLevel(Level.WARN)
  Logger.getLogger("com").setLevel(Level.WARN)

def runHotcellAnalysis(spark: SparkSession, pointPath: String): DataFrame =
{
  // Load the original data from a data source
  var pickupInfo = spark.read.format("com.databricks.spark.csv").option("delimiter",";").option("header","false").load(pointPath);
  pickupInfo.createOrReplaceTempView("nyctaxitrips")
  pickupInfo.show()

  // Assign cell coordinates based on pickup points
  spark.udf.register("CalculateX",(pickupPoint: String)=>((
    HotcellUtils.CalculateCoordinate(pickupPoint, 0)
    )))
  spark.udf.register("CalculateY",(pickupPoint: String)=>((
    HotcellUtils.CalculateCoordinate(pickupPoint, 1)
    )))
  spark.udf.register("CalculateZ",(pickupTime: String)=>((
    HotcellUtils.CalculateCoordinate(pickupTime, 2)
    )))
  pickupInfo = spark.sql("select CalculateX(nyctaxitrips._c5),CalculateY(nyctaxitrips._c5), CalculateZ(nyctaxitrips._c1) from nyctaxitrips")
  var newCoordinateName = Seq("x", "y", "z")
  pickupInfo = pickupInfo.toDF(newCoordinateName:_*)
  pickupInfo.show()

  // Define the min and max of x, y, z
  val minX = -74.50/HotcellUtils.coordinateStep
  val maxX = -73.70/HotcellUtils.coordinateStep
  val minY = 40.50/HotcellUtils.coordinateStep
  val maxY = 40.90/HotcellUtils.coordinateStep
  val minZ = 1
  val maxZ = 31
  val numCells = (maxX - minX + 1)*(maxY - minY + 1)*(maxZ - minZ + 1)

  // YOU NEED TO CHANGE THIS PART

  // return pickupInfo // YOU NEED TO CHANGE THIS PART
      spark.udf.register("sq", (inputX: Int) => HotcellUtils.sq(inputX))
    spark.udf.register("CountNeigh", (minX: Int, minY: Int, minZ: Int, maxX: Int, maxY: Int, maxZ: Int, inputX: Int, inputY: Int, inputZ: Int)
    => HotcellUtils.CalculateNeighbors(minX, minY, minZ, maxX, maxY, maxZ, inputX, inputY, inputZ))

    spark.udf.register("GScore", (x: Int, y: Int, z: Int, num_adjCells: Int, sum_adjCells: Int, numcells: Int, mean: Double, sd: Double) => HotcellUtils.calculateGScore(x, y, z, num_adjCells, sum_adjCells, numcells, mean, sd))

    val ip_Points = spark.sql("select x, y, z from pickupinfo where x >= " + minX + " and y >= " + minY  + " and z >= " + minZ + " and x <= " + maxX + " and y <= " + maxY +  " and z <= " + maxZ + " order by z, y, x").persist()
    ip_Points.createOrReplaceTempView("ip_Points")
    ip_Points.show()

    val count_Points = spark.sql("select x, y, z, count(*) as point_Value from ip_Points group by z, y, x order by z, y, x").persist()
    count_Points.createOrReplaceTempView("count_Points")
    count_Points.show()

    val sum_Points = spark.sql("select sum(point_Value) as sum_Value, sum(sq(point_Value)) as sqSum from count_Points")
    sum_Points.createOrReplaceTempView("sum_Points")
    sum_Points.show()

    val sum_Value = sum_Points.first().getLong(0)
    val sqSum = sum_Points.first().getDouble(1)

    val mean = sum_Value.toDouble / numCells.toDouble
    val SD = math.sqrt((sqSum.toDouble / numCells.toDouble) - (mean.toDouble * mean.toDouble))

    val Neighbors = spark.sql("select CountNeigh(" + minX + "," + minY + "," + minZ + "," + maxX + "," + maxY + "," + maxZ + "," + "a1.x, a1.y, a1.z) as num_n, count(*) as countAll, a1.x as x, a1.y as y, a1.z as z, sum(a2.point_Value) as total_Value from count_points as a1, count_Points as a2 where (a2.x = a1.x + 1 or a2.x = a1.x or a2.x = a1.x - 1) and (a2.y = a1.y + 1 or a2.y = a1.y or a2.y =a1.y - 1) and (a2.z = a1.z + 1 or a2.z = a1.z or a2.z = a1.z - 1) group by a1.z, a1.y, a1.x order by a1.z, a1.y, a1.x").persist()
    Neighbors.createOrReplaceTempView("NeighCount")
    Neighbors.show()

    val GScoreDF = spark.sql("select GScore(x, y, z, num_n, total_Value, "+ numCells + ", " + mean + ", " + SD + ") as gscore, x, y, z from NeighCount order by gscore desc");
    GScoreDF.createOrReplaceTempView("GScoreDF")
    GScoreDF.show()

    val lastresult = spark.sql("select x, y, z from GScoreDF")
    lastresult.createOrReplaceTempView("lastresult")
    lastresult.show()
    lastresult  
}
}
