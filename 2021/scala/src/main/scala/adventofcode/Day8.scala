package day8

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

val test_data = List(
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
  "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
  "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
  "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
  "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
  "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
  "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
  "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
  "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
  "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
)
val puzzle_data =
  Source.fromResource("adventofcode/day8.txt").getLines.toList

type Segment = Set[Char]
type SegmentLine = Seq[Segment]

def parse(data: String): (SegmentLine, SegmentLine) =
  data
    .split(' ')
    .span(_ != "|")
    .pipe((train, test) => (train.map(_.toSet).toSeq, test.drop(1).map(_.toSet).toSeq))

object Inference:
  def apply(segment_line: SegmentLine) =
    new Inference(Map(), segment_line.toSet)

case class Inference(known: Map[Int, Segment], segment_line: Set[Segment]):
  require(known.values.toSet.size == known.values.size, "Known values must be distinct")

  // Decoder lookup from a segment to number
  def decoder = known.map((k, v) => (v, k))

  // Infer `n` for `state` according to `by` assuming knowledge of `given`.
  def inferBy(n: Int, `given`: Int, by: Segment => Segment => Boolean) =
    require(known.contains(`given`), s"Require knowledge of ${`given`}")
    val segment = segment_line
      .filter(by(known(`given`)))
      .headOption match
      case None    => throw Exception(s"$n uninferred for $this")
      case Some(s) => s
    this.copy(known = known.updated(n, segment))

  // Infer what we can based on length
  def inferByLength =
    def lengthInference(x: Segment): Option[(Int, Segment)] = x.size match
      case 2 => Some((1, x))
      case 3 => Some((7, x))
      case 4 => Some((4, x))
      case 7 => Some((8, x))
      case _ => None
    this.copy(known = this.known ++ segment_line.flatMap(lengthInference))

def inferSegmentLineCoding(segment_line: SegmentLine): Inference =
  Inference(segment_line).inferByLength
    // 3 is the only length 5 that is a superset of 7
    .inferBy(3, `given` = 7, g => s => (s.size == 5) && (g forall s))
    // 9 is the only length 6 superset of 4
    .inferBy(9, `given` = 4, g => s => (s.size == 6) && (g forall s))
    // 2 is the only length 5 not a subset of 9
    .inferBy(2, `given` = 9, g => s => (s.size == 5) && !(s forall g))
    // 6 is the only length 6 to not have 1 as a subset
    .inferBy(6, `given` = 1, g => s => (s.size == 6) && !(g forall s))
    // 5 is the only length 5 subset of 6 WRONG
    .inferBy(5, `given` = 6, g => s => (s.size == 5) && (s forall g))
    // 0 is the only length 6 to not have 5 as a subset
    .inferBy(0, `given` = 5, g => s => (s.size == 6) && !(g forall s))

object part1:
  def apply(data: List[String]) =
    data
      .map(parse)
      .flatMap((xs, ys) => { // Flat list of outputs across all lines
        val decode_segment = inferSegmentLineCoding(xs).decoder
        ys.map(decode_segment(_))
      })
      .filter(Set(1, 4, 7, 8))
      .length

object part2:
  def apply(data: List[String]) =
    data
      .map(parse)
      .map((xs, ys) => { // Decode segment outputs
        val decode_segment = inferSegmentLineCoding(xs).decoder
        ys.map(decode_segment(_)).mkString.toInt
      })
      .sum

@main def run =
  println(part1(test_data))
  println(part2(test_data))
