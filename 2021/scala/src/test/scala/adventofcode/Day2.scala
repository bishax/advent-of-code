package day2

import org.junit.Test
import org.junit.Assert.*

class Day2:
  @Test def first(): Unit = 
    assertEquals(part1(test_data), 150)

  @Test def second(): Unit = 
    assertEquals(part2(test_data), 900)
