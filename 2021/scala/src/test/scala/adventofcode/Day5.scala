package day5

import org.junit.Test
import org.junit.Assert.*

class Day5:
  @Test def first(): Unit = 
    assertEquals(part1(test_data), 5)

  @Test def second(): Unit = 
    assertEquals(part2(test_data), 12)
