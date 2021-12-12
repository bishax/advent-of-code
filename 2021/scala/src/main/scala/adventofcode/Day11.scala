package day11

import scala.io.Source
import scala.collection.immutable.LazyList
import scala.util.chaining.scalaUtilChainingOps

val test_data = Seq(
  "5483143223",
  "2745854711",
  "5264556173",
  "6141336146",
  "6357385478",
  "4167524645",
  "2176841721",
  "6882881134",
  "4846848554",
  "5283751526"
)
val puzzle_data =
  Source.fromResource("adventofcode/day11.txt").getLines.toList

type Coord = (Int, Int)
case class OctoGrid(private val floor: Array[Array[Option[Int]]]):
  def width = floor.head.size
  def height = floor.size
  def apply(pt: (Int, Int)) = floor(pt._2)(pt._1)
  def neighboursOf(pt: (Int, Int)): Seq[Coord] =
    val (x, y) = pt
    Seq(
      (x + 1, y), // RIGHT
      (x - 1, y), // LEFT
      (x, y - 1), // UP
      (x, y + 1), // DOWN
      // Diagonals
      (x + 1, y + 1),
      (x + 1, y - 1),
      (x - 1, y - 1),
      (x - 1, y + 1)
    ).filterNot((x, y) => (x < 0 || x >= this.width) || (y < 0 || y >= this.height))

  // Increment all positions by one (ignoring None's)
  def increment = OctoGrid(
    floor.map(_.clone).map(line => line.map(pt => pt.map(_ + 1)))
  )

  // Increment @param coords by one (ignoring None's)
  def increment(coords: Seq[Coord]) =
    var newGrid = floor.map(_.clone)
    for (x, y) <- coords do newGrid(y)(x) = newGrid(y)(x).map(_ + 1)
    OctoGrid(newGrid)

  // Set @param coords to `None` to indicate they flashed this step
  def exhaust(coords: Seq[Coord]) =
    var newGrid = floor.map(_.clone)
    for (x, y) <- coords do newGrid(y)(x) = None
    OctoGrid(newGrid)

  // Set every None back to Zero
  def replenish =
    OctoGrid(
      floor
        .map(_.clone)
        .map(line =>
          line.map(pt =>
            pt match
              case None => Some(0)
              case pt   => pt
          )
        )
    )

def parse(raw_data: Seq[String]): OctoGrid =
  new OctoGrid(
    raw_data
      .map(line =>
        (line.map(pt => Some(pt.toString.toInt)).toArray: Array[Option[Int]])
      )
      .toArray
  )

def findFlashers(floorMap: OctoGrid) =
  for
    x <- (0 until floorMap.width)
    y <- (0 until floorMap.height)
    if floorMap(x, y).getOrElse(0) > 9
  yield (x, y)

def iterate[A](f: A => A, x: A): LazyList[A] =
  // @tailrec
  def inner[A](f: A => A, x: A): LazyList[A] =
    val y = f(x)
    y #:: inner(f, y)
  x #:: inner(f, x)

type State = (Int, OctoGrid)
def step(state: State, x: Int): State =
  val (nFlashes, octo) = state
  def inner: State => State = (nFlashes, octo) =>
    val flashers = findFlashers(octo)
    (
      nFlashes + flashers.size,
      octo.exhaust(flashers).increment(flashers.flatMap(octo.neighboursOf))
    )

  iterate(inner, (nFlashes, octo.replenish.increment))
    .takeWhile((_, octo) => findFlashers(octo).size > 0)
    .toList match
    case Nil => (nFlashes, octo.replenish.increment)
    case xs => {
      val (nFlashes, octo) = inner(
        xs.last
      ) // Need to do one more (takeWhile ignores one)!
      (nFlashes, octo)
    }

object part1:
  def apply(data: Seq[String]): Int =
    (1 to 100).foldLeft((0, parse(data)))(step)._1

object part2:
  def apply(data: Seq[String]): Int =
    iterate(x => step(x, 0), (0, parse(data)))
      .sliding(2)
      .map(_.toList)
      .takeWhile(res => (res(1)._1 - res(0)._1) != 100)
      .length + 1

@main def run =
  println(part1(test_data))
  println(part1(puzzle_data))
  println(part2(test_data))
  println(part2(puzzle_data))
