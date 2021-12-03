package day3

import org.junit.Test
import org.junit.Assert.*

class Day3:
  @Test def first(): Unit = 
    assertEquals(part1(test_data), 198)

  @Test def second(): Unit = 
    assertEquals(part2(test_data), 230)

