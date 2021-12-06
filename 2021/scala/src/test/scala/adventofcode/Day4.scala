package day4

import org.junit.Test
import org.junit.Assert.*

class Day4:
  @Test def first(): Unit = 
    assertEquals(part1(test_data), 4512)

  @Test def second(): Unit = 
    assertEquals(part2(test_data), 1924)


