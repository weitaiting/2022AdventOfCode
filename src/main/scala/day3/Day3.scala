import scala.io.Source

object Day3 {
    val lines: List[String] = Source.fromFile("src/main/scala/day3/input.txt").getLines().toList

    def getCommonCharInBothStrings(s1: String, s2: String): Char = {
        val secondString: Set[Char] = s2.toSet
        s1.find(secondString.contains).get
    }

    def getCommonCharInThreeStrings(s1: String, s2: String, s3: String): Char = {
        val secondString: Set[Char] = s2.toSet
        val thirdString: Set[Char] = s3.toSet
        s1.find(c => secondString.contains(c) && thirdString.contains(c)).get
    }

    def getCharScore(c: Char): Int = {
        // 97 is ASCII value of 'a'
        (c.toLower.toInt - 97 + 1) + (if (c.isUpper) 26 else 0)
    }

    def processLinePartOne(s: String): Int = {
        val (s1, s2) = s.splitAt(s.length / 2)
        val commonChar: Char = getCommonCharInBothStrings(s1, s2)
        getCharScore(commonChar)
    }

    def findPartOneSoln: Int = {
        lines.map(processLinePartOne).sum
    }

    def findPartTwoSoln: Int = {
        lines.grouped(3).map(items => getCharScore(getCommonCharInThreeStrings(items(0), items(1), items(2)))).sum
    }
}


@main def findDay3Soln: Unit = {
    val partOneSoln: Int = Day3.findPartOneSoln
    println("Part one solution is " + partOneSoln)
    val partTwoSoln: Int = Day3.findPartTwoSoln
    println("Part two solution is " + partTwoSoln)
}