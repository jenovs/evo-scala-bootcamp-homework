package ClassesAndTraits

import Homework._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HomeworkSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "Point" should "move" in {
    val point2D = Point(0, 0)
    point2D.x shouldEqual 0
    point2D.move(3, 2).x shouldEqual 3
    point2D.move(3, 2).move(dy = 3).y shouldEqual 5

    val point3D = Point(0, 0, 0)
    point3D.x shouldEqual 0
    point3D.move(5, 5).move(dz = 3).z shouldEqual 3
    point3D.move(3, 2).y shouldEqual 2
  }

  "Circle" should "move and have area" in {
    val circle = Circle(0, 0, 5)

    circle.move(4).x shouldEqual 4
    circle.move(1).move(1).x shouldEqual 2
    circle.move(4, 5).y shouldEqual 5
    circle.area shouldEqual 78.54 +- 0.001
  }

  "Rectangle" should "move and have area" in {
    val rect = Rectangle(0, 0, 5, 10)

    rect.move(3).x shouldEqual 3
    rect.move(3, 5).y shouldEqual 5
    rect.area shouldEqual 50
  }

  "Square" should "move and have area" in {
    val square = Square(0, 0, 7)

    square.move(3).move(-1).x shouldEqual 2
    square.move(3, 5).y shouldEqual 5
    square.area shouldEqual 49
  }

  "Triangle" should "move and have area" in {
    val triangle = Triangle(Vertex(0, 0), Vertex(5, 0), Vertex(5, 10))

    triangle.x shouldEqual 3.333 +- 0.001
    triangle.y shouldEqual 3.333 +- 0.001
    triangle.move(5).x shouldEqual 8.333 +- 0.001
    triangle.move(dy = 5).x shouldEqual 3.333 +- 0.001
    triangle.area shouldEqual 25.0 +- 0.001
  }

  "Sphere" should "move and have surface area and volume" in {
    val sphere = Sphere(0, 0, 0, 10)

    sphere.move(6).move(1).x shouldEqual 7
    sphere.move(dz = 5).y shouldEqual 0
    sphere.move(dz = 5).z shouldEqual 5
    sphere.surfaceArea shouldEqual 1256.637 +- 0.001
    sphere.volume shouldEqual 4188.79 +- 0.001
  }

  "Cube" should "move and have surface area and volume" in {
    val cube = Cube(0, 0, 0, 15)

    cube.move(6).move(1).x shouldEqual 7
    cube.move(dz = 5).y shouldEqual 0
    cube.move(dz = 5).z shouldEqual 5
    cube.surfaceArea shouldEqual 1350
    cube.volume shouldEqual 3375
  }

  "Cuboid" should "move and have surface area and volume" in {
    val cuboid = Cuboid(0, 0, 0, 15, 12, 10)

    cuboid.move(6).move(1).x shouldEqual 7
    cuboid.move(dz = 5).y shouldEqual 0
    cuboid.move(dz = 5).z shouldEqual 5
    cuboid.surfaceArea shouldEqual 900
    cuboid.volume shouldEqual 1800
  }
}
