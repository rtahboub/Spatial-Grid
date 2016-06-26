import java.awt.geom.Rectangle2D
import java.awt.geom.Point2D

case class Point (id: Int, x: Double, y: Double)

trait GridAttributes{
  val spaceSide = 22361
  val numAgents = 50000
  val numCells = (numAgents / 296).toInt
  val cellsPerSide = (math.sqrt(numCells)).toInt
  val cellSize = (spaceSide / cellsPerSide).toInt
}

class Grid[A <% Point] (pnts:List[A]) extends GridAttributes {
  var grid = makeGrid()

  def makeGrid(): Array[Array[List[A]]] = {
    val g = Array.fill(cellsPerSide, cellsPerSide)(Nil: List[A])

    for (p <- pnts) {
      val xbin = (p.x / cellSize).toInt
      val ybin = (p.y / cellSize).toInt
      g(xbin)(ybin) ::= p
    }
    g
  }

  def printGrid(): Unit = {
    for (i <- 0 to cellsPerSide - 1) {
      print(i, " ")
      for (j <- 0 to cellsPerSide - 1)
        print(grid(i)(j), " ")
      println()
    }
  }

  def nestedIndexWindowJoin(t1: List[Point], xDel: Double, yDel: Double): List[Point] = {
    var result = List[Point]()
    for (i <- 0 to t1.length - 1) {
      val window = new Rectangle2D.Double(t1(i).x - xDel, t1(i).y - yDel, 2 * xDel, 2 * yDel )
 //     println(t1(i).x, t1(i).y, window)

      for (a <- 0 to cellsPerSide - 1){
        for (b <- 0 to cellsPerSide - 1){
          val gridcell = new Rectangle2D.Double(cellSize * a, cellSize * b, cellSize, cellSize)
          if (window.contains(gridcell)){
 //           println("contains", a, b)
            //report all points
              result ::: grid(a)(b)
          }
          else if(window.intersects(gridcell)){
 //           println("intersects", a, b)
            for(c <- grid(a)(b))
              if(window.contains(new Point2D.Double(c.x,c.y))) {
                result ::= c
 //               println(t1(i).x, t1(i).y, c)
 //               println(result)
              }

          }
        }
      }
    }
    result
  }
}

object driver{
  def processCSV(filename: String): List[Point] = {
    var result = List[Point]()
    val in = new Scanner(filename)
    while (in.hasNext) {
      val id   = in.next(',').toInt
      val x   = in.next(',').toDouble
      val y  = in.next('\n').toDouble
      result ::= Point(id,x,y)
    }
    in.close

    result
  }

  def main(args: Array[String]): Unit = {
    val t1 = processCSV("src/data/test10_t1.csv")
    val t2 = processCSV("src/data/test10_t2.csv")
    val x = new Grid(t2)
    val r = x.nestedIndexWindowJoin(t1,11.26, 11.26)
    println(r)
  }
}