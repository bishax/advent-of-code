package day10

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

val test_data = Seq(
  "[({(<(())[]>[[{[]{<()<>>",
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]"
)
val puzzle_data =
  Source.fromResource("adventofcode/day10.txt").getLines.toList

type State = Either[Char, List[Char]]
val getClose: Map[Char, Char] = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

def parse(line: String): State =
  val init: State = Right(Nil)
  line.foldLeft(init)((acc, c) => {
    acc.fold(
      l => { Left(l) },
      r => {
        c match
          case ')' | ']' | '}' | '>' =>
            if c == getClose(r.head) then Right(r.tail) else Left(c)
          case '(' | '[' | '{' | '<' => Right(c :: r)
      }
    )
  })

def autocomplete(line: String): List[Char] =
  parse(line)
    .fold(l => throw Exception("Can't autocomplete corrupt line"), r => r)
    .map(getClose)

def midPoint[T](xs: Seq[T]): T = xs((xs.size) / 2)

object part1:
  val penalty = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  def apply(data: Seq[String]): Int =
    test_data.map(parse).map(_.fold(l => penalty(l), r => 0)).sum

object part2:
  def autocomplete_score(xs: Seq[Char]): Long =
    def inner(xs: Seq[Char], score: Long): Long = xs match
      case Nil       => score
      case ')' :: xs => inner(xs, score * 5 + 1)
      case ']' :: xs => inner(xs, score * 5 + 2)
      case '}' :: xs => inner(xs, score * 5 + 3)
      case '>' :: xs => inner(xs, score * 5 + 4)
    inner(xs, 0)

  def apply(data: Seq[String]): Long =
    data
      .filter(parse(_).fold(l => false, r => true))
      .map(autocomplete)
      .map(autocomplete_score)
      .sorted
      .pipe(midPoint(_))
