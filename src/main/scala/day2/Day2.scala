import scala.io.Source
import scala.collection.mutable.ListBuffer

object Moves extends Enumeration {
  type Move = Value
  val Rock, Paper, Scissors = Value
}

object RoundResults extends Enumeration {
  type Result = Value
  val Win, Lose, Draw = Value
}

def getLines(): List[String] = Source.fromFile("src/main/scala/day2/input.txt").getLines().toList

def decodeOpponentMove(c: Char): Moves.Move = c match {
  case 'A' => Moves.Rock
  case 'B' => Moves.Paper
  case 'C' => Moves.Scissors
}

def decodeYourMove(c: Char): Moves.Move = c match {
  case 'X' => Moves.Rock
  case 'Y' => Moves.Paper
  case 'Z' => Moves.Scissors
}

def decodeYourExpectedOutcome(c: Char): RoundResults.Result = c match {
  case 'X' => RoundResults.Lose
  case 'Y' => RoundResults.Draw
  case 'Z' => RoundResults.Win
}

def getRounds(lines: List[String]): List[(Moves.Move, Moves.Move)] = {
  lines.map(line => {
    val opponentMove: Moves.Move = decodeOpponentMove(line(0))
    val yourMove: Moves.Move = decodeYourMove(line(2))
    (opponentMove, yourMove)
  })
}

def getScoresPerRoundPart2(lines: List[String]): List[Int] = {
  lines.map(line => {
    val opponentMove: Moves.Move = decodeOpponentMove(line(0))
    val yourExpectedOutcome: RoundResults.Result = decodeYourExpectedOutcome(line(2))
    val yourMove: Moves.Move = calculateYourExpectedMove(opponentMove, yourExpectedOutcome)
    val score: Int = convertOutcomeToScore(yourExpectedOutcome) + calculateYourMoveBaseScore(yourMove)
    score
  })
}

def getMoveToWin(opponentMove: Moves.Move) = {
  opponentMove match {
    case Moves.Rock => Moves.Paper
    case Moves.Paper => Moves.Scissors
    case Moves.Scissors => Moves.Rock
  }
}

def getMoveToLose(opponentMove: Moves.Move) = {
  opponentMove match {
    case Moves.Rock => Moves.Scissors
    case Moves.Paper => Moves.Rock
    case Moves.Scissors => Moves.Paper
  }
}

def calculateYourExpectedMove(opponentMove: Moves.Move, yourExpectedOutcome: RoundResults.Result): Moves.Move = {
  yourExpectedOutcome match {
    case RoundResults.Draw => opponentMove
    case RoundResults.Win => getMoveToWin(opponentMove)
    case RoundResults.Lose => getMoveToLose(opponentMove)
  }
}

def calculateMatchOutcome(opponentMove: Moves.Move, yourMove: Moves.Move): RoundResults.Result = {
  opponentMove match {
    case Moves.Rock => yourMove match {
      case Moves.Rock => RoundResults.Draw
      case Moves.Paper => RoundResults.Win
      case Moves.Scissors => RoundResults.Lose
    }
    case Moves.Paper => yourMove match {
      case Moves.Rock => RoundResults.Lose
      case Moves.Paper => RoundResults.Draw
      case Moves.Scissors => RoundResults.Win
    }
    case Moves.Scissors => yourMove match {
      case Moves.Rock => RoundResults.Win
      case Moves.Paper => RoundResults.Lose
      case Moves.Scissors => RoundResults.Draw
    }
  }
}

def convertOutcomeToScore(outcome: RoundResults.Result): Int = {
  outcome match {
    case RoundResults.Win => 6
    case RoundResults.Lose => 0
    case RoundResults.Draw => 3
  }
}

def calculateYourMoveBaseScore(yourMove: Moves.Move): Int = {
  yourMove match {
    case Moves.Rock => 1
    case Moves.Paper => 2
    case Moves.Scissors => 3
  }
}

def calculateScore(opponentMove: Moves.Move, yourMove: Moves.Move): Int = {
    val yourMoveBaseScore: Int = calculateYourMoveBaseScore(yourMove)
    val matchOutcomeScore: Int =
      convertOutcomeToScore(calculateMatchOutcome(opponentMove, yourMove))
    yourMoveBaseScore + matchOutcomeScore
}

@main def findDay2Soln: Unit = {
  val lines: List[String] = getLines()
  val rounds: List[(Moves.Move, Moves.Move)] = getRounds(lines)
  val scoresPerRound: List[Int] = rounds.map({ case (m1, m2) => calculateScore(m1, m2)})
  println("Part one solution (total score) is: " + scoresPerRound.sum)
  println("part two solution (total score) is: " + getScoresPerRoundPart2(lines).sum)
}
