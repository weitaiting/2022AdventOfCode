import scala.io.Source
import scala.collection.mutable.ListBuffer

def getGroupedLines(): List[List[String]] = {
  var ret = new ListBuffer[List[String]]
  var l = new ListBuffer[String]
  for (line <- Source.fromFile("src/main/scala/day1/input.txt").getLines()) {
    if (line == "") {
      ret += l.toList
      l = new ListBuffer[String]
    } else {
      l += line
    }
  }
  if (!l.isEmpty) {
    ret += l.toList
  }
  ret.toList
}

@main def findSoln: Unit = {
  val groupedLines: List[List[String]] = getGroupedLines()
  val caloriesPerElf: List[Int] = groupedLines.map(numStrings => numStrings.map(_.toInt).sum)
  println("Part one solution is: " + caloriesPerElf.max)
  val top3ElfCaloriesSum: Int = caloriesPerElf.sorted.reverse.take(3).sum
  println("Part two solution is: " + top3ElfCaloriesSum)
}
