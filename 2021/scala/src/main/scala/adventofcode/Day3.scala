package day3

import scala.io.Source

val test_data = List(
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"
)
val puzzle_data =
  Source.fromResource("adventofcode/day3.txt").getLines.toList

case class BitCounter(zeros: Int, ones: Int):
  def mostCommon = if this.zeros > this.ones then '0' else '1'
  def leastCommon = if this.zeros <= this.ones then '0' else '1'

  def add(s: Char) =
    s match
      case '0' => this.copy(zeros = zeros + 1)
      case '1' => this.copy(ones = ones + 1)
      case _   => throw java.lang.Exception("Not possible")

def flip(x: Seq[Char]) = x.map(_ match
  case '1' => '0'
  case '0' => '1'
)

def binaryToInt(bin: String): Int = Integer.parseInt(bin, 2)

object part1:
  def apply(data: List[String]): Int =
    val counters = (1 to data.length).map(_ => BitCounter(0, 0))
    val countBitsByPos = (counters: Seq[BitCounter], bits: String) =>
      counters.zip(bits).map((counter, bit) => counter.add(bit))

    val binary_seq = data
      .foldLeft(counters)(countBitsByPos)
      .map(_.mostCommon)

    val gamma = binaryToInt(binary_seq.mkString)
    val epsilon = binaryToInt(flip(binary_seq).mkString)

    gamma * epsilon

object part2:
  type Component = "O2" | "CO2"

  def count_bits(bits: Seq[Char]): BitCounter =
    bits.foldLeft(BitCounter(0, 0))((counter, bit) => counter.add(bit))

  def getLifeSupportComponentRating(component: Component)(data: List[String]): Int =
    def inner(data: List[String], n: Int): String =
      val nth_bit_counts = count_bits(data.map(_(n)))
      val selector_bit = component match
        case "O2"  => nth_bit_counts.mostCommon
        case "CO2" => nth_bit_counts.leastCommon

      data.filter(_(n) == selector_bit) match
        case x :: Nil => x
        case xs       => inner(xs, n + 1)
    binaryToInt(inner(data, 0))

  def apply(data: List[String]): Int =
    val getO2GeneratorRating = getLifeSupportComponentRating("O2")
    val getCO2ScrubberRating = getLifeSupportComponentRating("CO2")

    getO2GeneratorRating(data) * getCO2ScrubberRating(data)
