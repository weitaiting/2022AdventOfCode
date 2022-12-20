import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.io.Source

object Day6 {
  def findFirstStartOfPacketMarkerPos(s: String, nDistinct: Int): Int = {
    var charMap: Map[Char, Int] = new HashMap[Char, Int]()
    var i: Int = 0
    while (i < s.length) {
//      println(i + charMap.toString)
      if (charMap.size == nDistinct) {
        return i
      } else  if (charMap.contains(s(i))) {
        var j = charMap.remove(s(i)).get
        charMap.put(s(i), i)
        j -= 1
        while (j >= 0 && charMap.contains(s(j)) && charMap.get(s(j)).get == j) {
//          println("Removing " + s(j) + " at pos " + j + ". remainder" + charMap.toString)
          charMap.remove(s(j))
          j -= 1
        }
      } else {
        charMap.put(s(i), i)
      }
      i += 1
    }
    i
  }

  val line: String = Source.fromFile("src/main/scala/day6/input.txt").getLines().toList(0)
}


@main def findDay6Soln: Unit = {
  val s1: Int = Day6.findFirstStartOfPacketMarkerPos(Day6.line, 4)
  println("Part one soln is " + s1)
  val s2: Int = Day6.findFirstStartOfPacketMarkerPos(Day6.line, 14)
  println("Part two soln is " + s2)
}