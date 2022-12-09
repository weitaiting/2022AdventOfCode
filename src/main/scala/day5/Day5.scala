import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Stack[T](items: List[T]) {
  def pop(): (T, Stack[T]) = (items.head, Stack(items.tail))
  def push(item: T): Stack[T] = Stack(item :: items)
  def popN(n: Int): (List[T], Stack[T]) = (items.slice(0, n), Stack(items.slice(n, items.size)))
}

case class Instruction(count: Int, from: Char, to: Char)

object Day5 {
  val lines: List[String] = Source.fromFile("src/main/scala/day5/input.txt").getLines().toList
  val crateInputLines: List[String] = lines.slice(0, 8)
  val crateLabelString: String = lines(8)
  val instructionLines: List[String] = lines.slice(10, lines.size)
  val instructionRegex = raw"move (\d{1,4}) from (\d{1}) to (\d{1})".r
  val parsedInstructions: List[Instruction] = instructionLines.map(l => l match {
    case instructionRegex(c, f, t) => Instruction(c.toInt, f(0), t(0))
  })

  def executeInstruction(m: scala.collection.mutable.Map[Char, Stack[Char]], i: Instruction, preserveOrder: Boolean): Unit = {
    println(i)
    if (preserveOrder) {
      val (itemsToMove, newStack) = m.get(i.from).get.popN(i.count)
      m.put(i.from, newStack)
      m.put(i.to, Stack(itemsToMove ++ m.get(i.to).get.items))
    } else {
      Range(0, i.count).foreach(_ => {
        val oldStack: Stack[Char] = m.get(i.from).get
        val (item, oldStack2) = oldStack.pop()
        m.put(i.from, oldStack2)
        val newStack2 = m.get(i.to).get.push(item)
        m.put(i.to, newStack2)
      })
    }
  } 

  def getLabelToCrates(): Map[Char, Stack[Char]] = {
    val charPositions: Range = Range(1, 34, 4)  
    charPositions.map(pos => {
      val label: Char = crateLabelString(pos)
      val crates: List[Char] = crateInputLines.map(l => l(pos)).filter(c => c != ' ')
      println(crates)
      label -> Stack(crates)
    }).toMap
  }

  def findSoln(preserveOrder: Boolean): String = {
    val labelToCrates = scala.collection.mutable.Map(getLabelToCrates().toSeq: _*)
    println(labelToCrates)
    parsedInstructions.foreach(instruction => {
        executeInstruction(labelToCrates, instruction, preserveOrder)
        println(labelToCrates)
    })
    labelToCrates.values.map(_.items.head).mkString
  }
}

@main def findDay5Soln: Unit = {
  val s1: String = Day5.findSoln(false)
  println("Part one soln is " + s1)
  val s2: String = Day5.findSoln(true)
  println("part two soln is " + s2)
}