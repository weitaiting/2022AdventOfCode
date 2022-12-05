
import scala.io.Source


object Day4 {
  case class Range(start: Int, end: Int) {
    def contains(r2: Range): Boolean = start <= r2.start && end >= r2.end

    def overlaps(r2: Range): Boolean = {
      (start >= r2.start && start <= r2.end)
      || (end >= r2.start && end <= r2.end)
    }
  }

  val lines: List[String] = Source.fromFile("src/main/scala/day4/input.txt").getLines().toList

  val ranges: List[(Range, Range)] = lines.map(convertToRanges)

  def convertToRanges(s: String): (Range, Range) = {
    val parts: Array[String] = s.split(",")
    (convertToRange(parts(0)), convertToRange(parts(1)))
  }

  def convertToRange(s: String): Range = {
    val parts: Array[String] = s.split("-")
    Range(parts(0).toInt, parts(1).toInt)
  }

  def findPartOneSoln: Int = {
    ranges.map((r1, r2) => if (r1.contains(r2) || r2.contains(r1)) 1 else 0).sum
  }

  def findPartTwoSoln: Int = {
    ranges.map((r1, r2) => if (r1.overlaps(r2) || r2.overlaps(r1)) 1 else 0).sum
  }
}   

@main def findDay4Soln: Unit = {
  val part1Soln = Day4.findPartOneSoln
  println("Part one solution is " + part1Soln)
  val part2Soln = Day4.findPartTwoSoln
  println("Part two solution is " + part2Soln)
}