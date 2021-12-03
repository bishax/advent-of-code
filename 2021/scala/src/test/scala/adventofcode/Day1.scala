import org.junit.Test
import org.junit.Assert.*

class Day1:
  @Test def part1(): Unit =
    assertEquals(day1.day1(day1.test_data, 1), 7)

  @Test def part2(): Unit =
    assertEquals(day1.day1(day1.test_data, 3), 5)
