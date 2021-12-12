package day11

import org.junit.Test
import org.junit.Assert.*

class Day11:
  @Test def first(): Unit =
    assertEquals(part1(test_data), 1656)

  @Test def second(): Unit =
    assertEquals(part2(test_data), 195)