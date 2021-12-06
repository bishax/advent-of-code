package day5

import scala.io.Source

val test_data = List(
  "0,9 -> 5,9",
  "8,0 -> 0,8",
  "9,4 -> 3,4",
  "2,2 -> 2,1",
  "7,0 -> 7,4",
  "6,4 -> 2,0",
  "0,9 -> 2,9",
  "3,4 -> 1,4",
  "0,0 -> 8,8",
  "5,5 -> 8,2"
)
val puzzle_data =
  Source.fromResource("adventofcode/day5.txt").getLines.toList

case class Point(x: Int, y: Int)

def parse(raw_data: List[String]): List[(Point, Point)] =
  def parseOne(line: String): (Point, Point) =
    line
      .split(" -> ")
      .map(_.split(",") match
        case x if x.length == 2 => Point(x(0).toInt, x(1).toInt)
        case _                  => throw Exception("Parse error!")
      )
      .toList match
      case (l: Point) :: (r: Point) :: Nil => (l, r)
      case _                               => throw Exception("Parse error!")
  raw_data.map(parseOne)

def update_map(
    linepath: ((Point, Point)) => Option[Seq[Point]]
)(vent_map: Array[Array[Int]], line: (Point, Point)) =
  linepath(line) match
    case None => None
    case Some(points) =>
      for point <- points do vent_map(point.y)(point.x) += 1

def makeVentMap(lines: Seq[(Point, Point)]) =
  val width = lines.map(l => l._1.x.max(l._2.x)).max + 1
  val height = lines.map(l => l._1.y.max(l._2.y)).max + 1
  val size = width.max(height)
  Array.ofDim[Int](size, size)

object part1:
  def linepath(line: (Point, Point)): Option[Seq[Point]] =
    line match
      case (Point(x1, y1), Point(x2, y2)) if x1 == x2 =>
        Some(for y <- (y1 to y2 by y2.compare(y1)) yield Point(x1, y))
      case (Point(x1, y1), Point(x2, y2)) if y1 == y2 =>
        Some(for x <- (x1 to x2 by x2.compare(x1)) yield Point(x, y1))
      case _ => None

  def apply(raw_data: List[String]): Int =
    val lines = parse(raw_data)
    val vent_map = makeVentMap(lines)
    for line <- lines do update_map(linepath)(vent_map, line)
    vent_map.flatten.filter(_ > 1).length

object part2:
  def linepath(line: (Point, Point)): Option[Seq[Point]] =
    line match
      case (Point(x1, y1), Point(x2, y2)) if x1 == x2 =>
        Some(for y <- (y1 to y2 by y2.compare(y1)) yield Point(x1, y))
      case (Point(x1, y1), Point(x2, y2)) if y1 == y2 =>
        Some(for x <- (x1 to x2 by x2.compare(x1)) yield Point(x, y1))
      case (Point(x1, y1), Point(x2, y2)) if (x1 - x2).abs == (y1 - y2).abs =>
        Some(
          for (x, y) <- (x1 to x2 by x2.compare(x1)).zip(y1 to y2 by y2.compare(y1))
          yield Point(x, y)
        )
      case _ => None

  def apply(raw_data: List[String]): Int =
    val lines = parse(raw_data)
    val vent_map = makeVentMap(lines)
    for line <- lines do update_map(linepath)(vent_map, line)
    vent_map.flatten.filter(_ > 1).length

@main def run() =
  println(part1(test_data))
  println(part1(puzzle_data))
  println("====")
  println(part2(test_data))
  println(part2(puzzle_data))
