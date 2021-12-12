package day10

import org.junit.Test
import org.junit.Assert.*

class Day10:
  @Test def first(): Unit =
    assertEquals(part1(test_data), 26397)

  @Test def second(): Unit =
    assertEquals(part2(test_data), 288957)



