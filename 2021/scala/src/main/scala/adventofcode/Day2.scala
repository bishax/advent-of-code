package day2

import scala.io.Source

val test_data =
  List("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
val puzzle_data =
  Source.fromResource("adventofcode/day2.txt").getLines.toList

// Utility
def split_on_first_space(x: String) =
  val (x1, x2) = x.span(_ != ' ')
  (x1, x2.strip)

// Common domain model
enum Command:
  case Forward(n: Int)
  case Down(n: Int)
  case Up(n: Int)

case class Position(x: Int, depth: Int):
  def product = x * depth

def parse_data(data: List[String]): List[Command] =
  def parse_one(x: String) =
    val (dir_str, n_str) = split_on_first_space(x)
    val n = n_str.toInt
    dir_str match
      case "forward" => Command.Forward(n)
      case "up"      => Command.Up(n)
      case "down"    => Command.Down(n)
  data.map(parse_one)

object part1:
  def update_position(position: Position, command: Command): Position =
    command match
      case Command.Forward(n) => position.copy(x = position.x + n)
      case Command.Up(n)      => position.copy(depth = position.depth - n)
      case Command.Down(n)    => position.copy(depth = position.depth + n)

  def apply(puzzle_data: List[String]): Int =
    val initial_pos = Position(0, 0)
    val commands = parse_data(puzzle_data)
    commands.foldLeft(initial_pos)(update_position).product

object part2:
  type Aim = Int

  def update_position(
      state: (Aim, Position),
      command: Command
  ): (Aim, Position) =
    val (aim, position) = state
    command match
      case Command.Forward(n) =>
        (aim, Position(position.x + n, position.depth + aim * n))
      case Command.Up(n)   => (aim - n, position)
      case Command.Down(n) => (aim + n, position)

  def apply(puzzle_data: List[String]): Int =
    val initial_state = (0, Position(0, 0))
    val (_, final_pos) =
      parse_data(puzzle_data).foldLeft(initial_state)(update_position)
    final_pos.product
