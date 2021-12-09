package day9

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

val test_data = Seq(
  "2199943210",
  "3987894921",
  "9856789892",
  "8767896789",
  "9899965678"
)
val puzzle_data =
  Source.fromResource("adventofcode/day9.txt").getLines.toList

type Coord = (Int, Int)
case class FloorMap(private val floor: Seq[Seq[Int]]):
  def width = floor.head.size
  def height = floor.size
  def apply(pt: (Int, Int)) = floor(pt._2)(pt._1)
  def neighboursOf(pt: (Int, Int)): Seq[Coord] =
    val (x, y) = pt
    Seq((x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1))
      .filterNot((x, y) => (x < 0 || x >= this.width) || (y < 0 || y >= this.height))

def parse(raw_data: Seq[String]): FloorMap =
  new FloorMap(raw_data.map(line => line.map(_.toString.toInt)))

def findLows(floorMap: FloorMap) =
  for
    x <- (0 until floorMap.width)
    y <- (0 until floorMap.height)
    w = floorMap(x, y)
    if floorMap.neighboursOf(x, y).forall(floorMap(_) > w)
  yield (x, y)

object part1:
  def apply(data: Seq[String]): Int =
    val floorMap = parse(data)
    findLows(floorMap).map(floorMap(_) + 1).sum

def growBasin(floorMap: FloorMap)(low: Coord): Set[Coord] =
  def inner(pointQueue: List[Coord], basinMembers: Set[Coord]): Set[Coord] =
    pointQueue match
      case Nil => basinMembers
      case pt :: pts =>
        val newBasinMembers =
          floorMap
            .neighboursOf(pt)
            .filter(nbr_pt => floorMap(nbr_pt) > floorMap(pt) && floorMap(nbr_pt) < 9)
        inner((pts ++ newBasinMembers), (basinMembers ++ newBasinMembers) + pt)
  inner(List(low), Set())

object part2:
  def apply(data: Seq[String]): Int =
    val floorMap = parse(data)
    val growBasin_ = growBasin(floorMap)
    findLows(floorMap)
      .map(growBasin_(_).size)
      .sortBy(-_)
      .take(3)
      .reduce(_ * _)

@main def run =
  println(part1(test_data))
  println(part1(puzzle_data))
  println(part2(test_data))
  println(part2(puzzle_data))
