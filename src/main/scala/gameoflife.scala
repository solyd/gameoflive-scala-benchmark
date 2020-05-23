import java.time.{LocalDate, LocalDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.Random
import java.util.concurrent.TimeUnit
import java.util.logging.Logger


import scala.collection.mutable
import scala.concurrent.duration.Duration

object gameoflife extends App {
  def impl_v2(): Unit = {
    println(s"--- loaded @ ${LocalDateTime.now(ZoneOffset.UTC)} UTC ---")
    println(s"args: ${args.mkString(",")}")

    def add(x: Int)(v: Int) = x+v


    class Grid()

    class GameLoop {
      var tsLast: Double = 0
      var fpsCalcLast: Double = 0

      val calcFpsInterval = Duration(100, TimeUnit.MILLISECONDS)

      var frameTimes = mutable.ArrayBuffer.empty[Double]
      val FpsLimit = 60
      frameTimes.sizeHint((calcFpsInterval.toSeconds * FpsLimit).toInt)

      var avgFrameTimeToDispaly: Double = 0
      var fpsToDisplay: Double = 0


      val gridUpdateInterval = Duration(1, TimeUnit.SECONDS)
      var gridLastUpdate: Double = 0
      var gridGeneration = 0

      val size = 10

      object canvas {
        val width = 1152
        val height = 635
      }

      val rnd = new Random(214310)
      var grid_sz = Array.ofDim[Boolean](
        canvas.width / size * size,
        canvas.height / size * size
      )


      val grid = for {
        x <- 0 to canvas.width / size * size
        y <- 0 to canvas.height / size * size
      } yield (x, y)

      //println("grid size", grid.size, grid(0).size)

      // gen live cells
//      for (_ <- 0 to 100) {
//        grid(rnd.nextInt(grid.size))(rnd.nextInt(grid(0).size)) = true
//      }

      def add(x: Int)(v: Int) = x+v

      def neighbours(pos: (Int, Int)): Seq[(Int, Int)] = {
        (for {
          p1 <- Seq(add(1) _, add(0) _, add(-1) _)
          p2 <- Seq(add(1) _, add(0) _, add(-1) _)
        } yield (p1, p2))
          .map { case (f1, f2) => (f1(pos._1), f2(pos._2)) }
          .filter(p => p != (0, 0) && p._1 >= 0 && p._2 >= 0 && p._1 < grid_sz.size && p._2 < grid_sz(0).size)
      }

      var alive = (1 to 100).map { _ =>
        (
          math.abs((rnd.nextInt() % canvas.width) / size * size),
          math.abs((rnd.nextInt() % canvas.height) / size * size)
        )
      }.distinct


      def render(ts: Double): Unit = while (true) {
        frameTimes.append(ts - tsLast)
        tsLast = ts

        gridLastUpdate = ts
        gridGeneration += 1

        println(s"calculating generation #${gridGeneration} @ ${LocalDateTime.now()}")

        alive = alive.map[Option[(Int, Int)]] { pos =>
          (alive intersect neighbours(pos)).size match {
            case i if i >= 2 && i <= 3 => Some(pos)
            case _ => None
          }
        }.filter(_.nonEmpty)
          .map(_.get)

        alive ++= (grid diff alive).map[Option[(Int, Int)]] { pos =>
          if ((alive intersect neighbours(pos)).size == 3) {
            Some(pos)
          } else {
            None
          }
        }.filter(_.nonEmpty)
          .map(_.get)
      }
    }

    (new GameLoop).render(0)
  }

  impl_v2()
}
