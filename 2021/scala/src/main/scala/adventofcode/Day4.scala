package day4

import scala.io.Source
import scala.annotation.unchecked.uncheckedVariance

val test_data = List(
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
  "",
  "22 13 17 11  0",
  " 8  2 23  4 24",
  "21  9 14 16  7",
  " 6 10  3 18  5",
  " 1 12 20 15 19",
  "",
  " 3 15  0  2 22",
  " 9 18 13 17  5",
  "19  8  7 25 23",
  "20 11 10 24  4",
  "14 21 16 12  6",
  "",
  "14 21 17 24  4",
  "10 16 15  9 19",
  "18  8 23 26 20",
  "22 11 13  6  5",
  " 2  0 12  3  7"
)

val puzzle_data =
  Source.fromResource("adventofcode/day4.txt").getLines.toList

type Board = Seq[Seq[Int]]

def parse(raw_data: List[String]): (List[Int], List[Board]) =
  def line_to_ints(sep: Char)(x: String): List[Int] =
    x.split(sep).flatMap(_.toIntOption).toList

  val init = List.empty[String] :: Nil
  val groups = raw_data.foldRight(init) { (line, groups) =>
    if line == "" then List.empty[String] :: groups
    else (line :: groups.head) :: groups.tail
  }

  val draws = line_to_ints(',')(groups.head.head)

  val boards: List[Board] = groups.tail.map(_.map(line_to_ints(' ')))
  (draws, boards)

// Return `true` if @param board wins
def board_wins(board: Board, draws: Seq[Int]): Boolean =
  val winning_rows =
    (board ++ board.transpose).filter(line => line.forall(draws.contains(_)))
  !winning_rows.isEmpty

// Optionally return index of @param draws that board wins at
def board_wins_at(board: Board, draws: Seq[Int]): Option[Int] =
  // Currently this is O(n) - a simple strategy would be to bisect draws
  (0 until draws.length)
    .filter(idx => board_wins(board, draws.take(idx)))
    .headOption

def unmarked_numbers(board: Board, draws: Seq[Int]) =
  board.flatten.filterNot(draws.toSet)

def board_score(board: Board, draws: Seq[Int]) =
  unmarked_numbers(board, draws).sum * draws.last

def getWinnersBy[B](by: ((Option[Int] @uncheckedVariance, Int)) => B)(
    draws: Seq[Int],
    boards: Seq[Board]
)(implicit ord: Ordering[B]): Option[(Int, Int)] =
  boards
    .map(board_wins_at(_, draws))
    .zipWithIndex
    .sortBy(by)
    .head match {
    case (None, _)                   => None
    case (Some(n_moves), winner_idx) => Some((n_moves, winner_idx))
  }

object part1:
  def apply(raw_data: List[String]): Int =
    val (draws, boards) = parse(raw_data)
    val getFirstWinner = getWinnersBy(identity)
    val (n_moves, winner_idx) =
      getFirstWinner(draws, boards).getOrElse(throw Exception("No winners!"))
    println(s"Board $winner_idx wins in $n_moves")
    board_score(boards(winner_idx), draws.take(n_moves))

object part2:
  def apply(raw_data: List[String]): Int =
    val (draws, boards) = parse(raw_data)
    val getLastWinner = getWinnersBy(x => x._1.map(-_))
    val (n_moves, winner_idx) =
      getLastWinner(draws, boards).getOrElse(throw Exception("No winners!"))
    println(s"Board $winner_idx wins in $n_moves")
    board_score(boards(winner_idx), draws.take(n_moves))

@main def run() =
  println("TEST\n====")
  println(s"Answer: ${part1(test_data)}")
  println(s"Answer: ${part2(test_data)}")
  println("\nREAL\n====")
  println(s"Answer: ${part1(puzzle_data)}")
  println(s"Answer: ${part2(puzzle_data)}")
