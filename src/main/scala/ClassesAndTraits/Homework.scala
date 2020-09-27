package ClassesAndTraits

import java.awt.geom.Area

object Homework extends App {
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.

  sealed trait Located {
    def x: Double
    def y: Double
    def z: Double = 0
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double = 0
    def maxZ: Double = 0
  }

  sealed trait Movable {
    def move(dx: Double = 0, dy: Double = 0, dz: Double = 0): Shape =
      this match {
        case Point(x, y, z) => Point(x + dx, y + dy, z + dz)
        // 2D Shapes
        case Circle(x, y, r)       => Circle(x + dx, y + dy, r)
        case Rectangle(x, y, h, w) => Rectangle(x + dx, y + dy, h, w)
        case Square(x, y, size)    => Square(x + dx, y + dy, size)
        case Triangle(v1, v2, v3) =>
          Triangle(
            Vertex(v1.x + dx, v1.y + dy),
            Vertex(v2.x + dx, v2.y + dy),
            Vertex(v3.x + dx, v3.y + dy)
          )
        // 3D Shapes
        case Cube(x, y, z, size)      => Cube(x + dx, y + dy, z + dz, size)
        case Cuboid(x, y, z, w, h, l) => Cuboid(x + dx, y + dy, z + dz, w, h, l)
        case Sphere(x, y, z, r)       => Sphere(x + dx, y + dy, z + dz, r)
      }
  }

  sealed trait Areable {
    def area: Double
  }

  sealed trait Shape extends Located with Bounded with Movable

  final case class Point(x: Double, y: Double, override val z: Double = 0)
      extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
  }

  final case class Circle(x: Double, y: Double, r: Double)
      extends Shape
      with Areable {
    override def minX: Double = x - r
    override def maxX: Double = x + r
    override def minY: Double = y - r
    override def maxY: Double = y + r
    override def area: Double = math.Pi * r * r
  }

  sealed trait Rectangular extends Shape with Areable {
    def x: Double
    def y: Double
    def w: Double
    def h: Double
    def minX: Double = x - w / 2
    def maxX: Double = x + w / 2
    def minY: Double = y - h / 2
    def maxY: Double = y + h / 2
    override def area: Double = w * h
  }

  case class Rectangle(
      val x: Double,
      val y: Double,
      val h: Double,
      val w: Double
  ) extends Rectangular {
    override def minX: Double = x - w / 2
    override def maxX: Double = x + w / 2
    override def minY: Double = y - h / 2
    override def maxY: Double = y + h / 2
  }

  case class Square(
      x: Double,
      y: Double,
      size: Double
  ) extends Rectangular {
    override val h = size
    override val w = size
  }

  case class Vertex(x: Double, y: Double)

  final case class Triangle(
      v1: Vertex,
      v2: Vertex,
      v3: Vertex
  ) extends Shape
      with Areable {
    override val x = (v1.x + v2.x + v3.x) / 3
    override val y = (v1.y + v2.y + v3.y) / 3
    override def minX: Double = v1.x min v2.x min v3.x
    override def maxX: Double = v1.x max v2.x max v3.x
    override def minY: Double = v1.y min v2.y min v3.y
    override def maxY: Double = v1.y max v2.y max v3.y
    override def area: Double = {
      val side1 = math.sqrt(math.pow(v1.x - v2.x, 2) + math.pow(v1.y - v2.y, 2))
      val side2 = math.sqrt(math.pow(v2.x - v3.x, 2) + math.pow(v2.y - v3.y, 2))
      val side3 = math.sqrt(math.pow(v3.x - v1.x, 2) + math.pow(v3.y - v1.y, 2))
      val s = (side1 + side2 + side3) / 2
      math.sqrt(s * (s - side1) * (s - side2) * (s - side3))
    }
  }

  sealed trait Surfaceable {
    def surfaceArea: Double
  }

  sealed trait Volumeable {
    def volume: Double
  }

  case object Origin {
    def x: Double = 0
    def y: Double = 0
    def z: Double = 0
  }

  final case class Sphere(
      x: Double,
      y: Double,
      override val z: Double,
      r: Double
  ) extends Shape
      with Surfaceable
      with Volumeable {
    override def minX: Double = x - r
    override def maxX: Double = x + r
    override def minY: Double = y - r
    override def maxY: Double = y + r
    override def minZ: Double = z - r
    override def maxZ: Double = z + r
    override def surfaceArea: Double = 4 * math.Pi * r * r
    override def volume: Double = (4 * math.Pi * r * r * r) / 3
  }

  sealed trait Cubelike extends Shape with Surfaceable with Volumeable {
    def w: Double
    def h: Double
    def l: Double
    def minX: Double = x - w / 2
    def maxX: Double = x + w / 2
    def minY: Double = y - h / 2
    def maxY: Double = y + h / 2
    override def maxZ: Double = z + l / 2
    override def minZ: Double = z - l / 2
    def surfaceArea: Double = 2 * (w * h + w * l + h * l)
    def volume: Double = w * h * l
  }

  final case class Cube(
      x: Double,
      y: Double,
      override val z: Double,
      size: Double
  ) extends Cubelike {
    override def w: Double = size
    override def h: Double = size
    override def l: Double = size
  }

  final case class Cuboid(
      x: Double,
      y: Double,
      override val z: Double,
      w: Double,
      h: Double,
      l: Double
  ) extends Cubelike {}
}
