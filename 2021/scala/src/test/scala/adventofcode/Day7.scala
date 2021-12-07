package day7

import org.junit.Test
import org.junit.Assert.*

class Day7:
  @Test def first(): Unit =
    assertEquals(part1(test_data), 37)

  @Test def second(): Unit =
    assertEquals(part2(test_data), 168)
