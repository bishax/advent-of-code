package day6

import org.junit.Test
import org.junit.Assert.*

class Day6:
  @Test def first(): Unit = 
    assertEquals(part1(test_data, 80), 5934)

  @Test def second(): Unit = 
    val x: BigInt = 26984457539L
    assertEquals(part2(test_data, 256), x)
