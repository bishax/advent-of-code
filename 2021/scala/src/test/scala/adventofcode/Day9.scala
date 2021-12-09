package day9

import org.junit.Test
import org.junit.Assert.*

class Day9:
  @Test def first(): Unit =
    assertEquals(part1(test_data), 15)

  @Test def second(): Unit =
    assertEquals(part2(test_data), 1134)


