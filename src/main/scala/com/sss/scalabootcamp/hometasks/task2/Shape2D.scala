package com.sss.scalabootcamp.hometasks.task2

import java.lang.Math.{PI, abs, cos, max, min, pow, sqrt}

object Shape2D {

  sealed trait Shape[A <: Shape[A]] extends Located with Bounded with Movable[A] with Area

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Area {
    def area: Double
  }

  sealed trait Movable[A <: Shape[A]] {
    def move(dx: Double, dy: Double): A
  }

  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // Add method `area` to 2D shapes.

  final case class Triangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) extends Shape[Triangle] {
    override def x: Double = (x1 + x2 + x3) / 3
    override def y: Double = (y1 + y2 + y3) / 3

    override def minX: Double = min(min(x1, x2), x3)
    override def maxX: Double = max(max(x1, x2), x3)
    override def minY: Double = min(min(y1, y2), y3)
    override def maxY: Double = max(max(y1, y2), y3)

    override def move(dx: Double, dy: Double): Triangle = Triangle(x1 + dx, y1 + dy, x2 + dx, y2 + dy, x3 + dx, y3 + dy)

    override def area: Double = {
      val a = sqrt(pow(abs(x1 - x2), 2) + pow(abs(y1 - y2), 2))
      val b = sqrt(pow(abs(x2 - x3), 2) + pow(abs(y2 - y3), 2))
      val c = sqrt(pow(abs(x3 - x1), 2) + pow(abs(y3 - y1), 2))
      val p = (a + b + c) / 2
      sqrt(p * (p - a) * (p - b) * (p - c))
    }
  }

  final case class Square(centerX: Double, centerY: Double, sideSize: Double, angle: Double) extends Shape[Square] {
    override def x: Double = centerX
    override def y: Double = centerY

    override def minX: Double = x - abs(sideSize / 2 * cos(PI /4 + angle))
    override def maxX: Double = x + abs(sideSize / 2 * cos(PI /4 + angle))
    override def minY: Double = y - abs(sideSize / 2 * cos(PI /4 + angle))
    override def maxY: Double = y + abs(sideSize / 2 * cos(PI /4 + angle))

    override def move(dx: Double, dy: Double): Square = Square(centerX + dx, centerY + dy, sideSize, angle)

    override def area: Double = sideSize * sideSize
  }



}
