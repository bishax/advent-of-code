package day8

import org.junit.Test
import org.junit.Assert.*

class Day8:
  @Test def first(): Unit =
    assertEquals(part1(test_data), 26)

  @Test def second(): Unit =
    assertEquals(part2(test_data), 61229)

