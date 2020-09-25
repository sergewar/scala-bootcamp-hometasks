package com.sss.scalabootcamp.hometasks.task1

import scala.annotation.tailrec
// Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
// https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

object Task1 {
//  def main(args: Array[String]): Unit = {
//    println(lcm(3, 5))
//    println(gcd(11, 3))
//  }

  def lcm(a: Int, b: Int): Int = if (a == 0 && b == 0) 0 else Math.abs(a * b) / gcd(a, b)

  def gcd(a: Int, b: Int): Int = (a, b) match {
    case (0, b) => b
    case (a, 0) => a
    case (a, b) if a == b => a
    case (a, b) if a == 1 || b == 1 => 1
    case (a, b) if a % 2 == 0 && b % 2 == 0 => 2 * gcd(a / 2, b / 2)
    case (a, b) if a % 2 == 0 && b % 2 != 0 => gcd(a / 2, b)
    case (a, b) if a % 2 != 0 && b % 2 == 0 => gcd(a, b / 2)
    case (a, b) if a % 2 != 0 && b % 2 != 0 && a < b => gcd(b - a, a)
    case (a, b) if a % 2 != 0 && b % 2 != 0 && a > b => gcd(a - b, b)
  }
}

