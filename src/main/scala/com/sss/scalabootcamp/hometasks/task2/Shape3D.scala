package com.sss.scalabootcamp.hometasks.task2

import java.lang.Math.{PI, abs, cos}

object Shape3D {

  sealed trait Shape3D[A <: Shape3D[A]] extends Located with Bounded with Movable[A] with SurfaceArea with Volume

  sealed trait Located {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait SurfaceArea {
    def surfaceArea: Double
  }

  sealed trait Volume {
    def volume: Double
  }

  sealed trait Movable[A >: Shape3D[A]] {
    def move(dx: Double, dy: Double, dz: Double): A
  }

  // Homework

  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not


  final case class Point(x: Double, y: Double, z: Double) extends Shape3D[Point] {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def minZ: Double = z

    override def maxZ: Double = z

    override def surfaceArea: Double = 0

    override def volume: Double = 0

    override def move(dx: Double, dy: Double, dz: Double): Point = Point(x + dx, y + dx, z + dz)
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D[Point] {
    override def minX: Double = x - radius

    override def maxX: Double = x + radius

    override def minY: Double = y - radius

    override def maxY: Double = y + radius

    override def minZ: Double = z - radius

    override def maxZ: Double = z + radius

    override def surfaceArea: Double = 4 * PI * radius * radius

    override def volume: Double = 4 / 3 * PI * radius * radius * radius

    override def move(dx: Double, dy: Double, dz: Double): Point = Point(x + dx, y + dx, z + dz)
  }

  final case class Cube(x: Double, y: Double, z: Double,
                        sideSize: Double,
                        xa: Double, ya: Double, za: Double) extends Shape3D[Cube] {
    override def minX: Double = x - abs(sideSize / 2 * cos(PI / 4 + xa))

    override def maxX: Double = x + abs(sideSize / 2 * cos(PI / 4 + xa))

    override def minY: Double = y - abs(sideSize / 2 * cos(PI / 4 + ya))

    override def maxY: Double = y + abs(sideSize / 2 * cos(PI / 4 + ya))

    override def minZ: Double = z - abs(sideSize / 2 * cos(PI / 4 + za))

    override def maxZ: Double = z + abs(sideSize / 2 * cos(PI / 4 + za))

    override def surfaceArea: Double = 6 * sideSize * sideSize

    override def volume: Double = sideSize * sideSize * sideSize

    override def move(dx: Double, dy: Double, dz: Double): Cube = this.copy(x = x + dx, y = y + dx, z = z + dz)
  }

  final case class Cuboid(x: Double, y: Double, z: Double,
                          a: Double, b: Double, c: Double,
                          xa: Double, ya: Double, za: Double) extends Shape3D[Cuboid] {
    override def minX: Double = x - abs(a / 2 * cos(PI / 4 + xa))

    override def maxX: Double = x + abs(a / 2 * cos(PI / 4 + xa))

    override def minY: Double = y - abs(b / 2 * cos(PI / 4 + ya))

    override def maxY: Double = y + abs(b / 2 * cos(PI / 4 + ya))

    override def minZ: Double = z - abs(c / 2 * cos(PI / 4 + za))

    override def maxZ: Double = z + abs(c / 2 * cos(PI / 4 + za))

    override def surfaceArea: Double = 2 * a * a * b * b * c * c

    override def volume: Double = a * b * c

    override def move(dx: Double, dy: Double, dz: Double): Cuboid = this.copy(x = x + dx, y = y + dx, z = z + dz)
  }

}
