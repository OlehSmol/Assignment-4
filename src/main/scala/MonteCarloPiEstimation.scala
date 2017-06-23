package parallel

import org.scalameter

import scala.util.Random
import org.scalameter._

object MonteCarloPiEstimation{
  def pi(totalNumberOfPoints: Int): Double = 4.0*CountPointsInsideCircle(totalNumberOfPoints)/totalNumberOfPoints;

  def CountPointsInsideCircle(totalNumberOfPoints: Int): Int = {
    val randX = new Random
    val randY = new Random

    def simulate(hits: Int, pointsGenerated: Int): Int =
      if(pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = randX.nextDouble
        val y = randY.nextDouble

        simulate(hits + (if (x*x + y*y <= 1) 1 else 0), pointsGenerated + 1)
      }
    simulate(0, 0)
  }

  def piPar(totalNumberOfPoints: Int) = {
    val (pi1, pi2) = parallel(CountPointsInsideCircle(totalNumberOfPoints/2), CountPointsInsideCircle(totalNumberOfPoints/2))

    4.0*(pi1 + pi2)/totalNumberOfPoints
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 500000
    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer (new Warmer.Default)

    val seqtime = standardConfig measure {
      pi(totalNumberOfPoints)
    }

    val partime = standardConfig measure {
      piPar(totalNumberOfPoints)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")

    println(s"speedup: ${seqtime.value / partime.value} ms")
  }

}
