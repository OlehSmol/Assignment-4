package parallel

import org.scalameter

import scala.util.Random
import org.scalameter._

object MonteCarloNumericalIntergration{
  def Integration(f: Double => Double, from: Double, to: Double, n: Int): Double = CountSquere(f, from, to, n)/n

  def CountSquere(f: Double => Double, from: Double, to: Double, n: Int): Double = {
    val rand = new Random

    def simulate(sum: Double, i: Int): Double =
      if(i >= n)
        sum
      else {
        simulate(sum + f(from + rand.nextDouble*(to-from))*(to-from), i + 1)
      }
    simulate(0, 0)
  }

  def IntegrationPar(f: Double => Double, from: Double, to: Double, n: Int) = {
    val (pi1, pi2) = parallel(CountSquere(f, from, to, n/2), CountSquere(f, from, to, n/2))

    (pi1 + pi2)/n
  }

  def IntegrationDoublePar(f: Double => Double, from: Double, to: Double, n: Int) = {
    val (pi1, pi2, pi3, pi4) = parallel(
      CountSquere(f, from, to, n/4),
      CountSquere(f, from, to, n/4),
      CountSquere(f, from, to, n/4),
      CountSquere(f, from, to, n/4)
    )
    (pi1 + pi2 + pi3 + pi4)/n
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 500000
    val from = -20
    val to = 20

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer (new Warmer.Default)

    def function(x: Double): Double = {
      math.pow(x, 3) - math.cos(x)*math.pow(x, 2)
    }

    val seqtime = standardConfig measure {
      Integration(function, from, to, totalNumberOfPoints)
    }

    val partime = standardConfig measure {
      IntegrationDoublePar(function, from, to, totalNumberOfPoints)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")

    println(s"speedup: ${seqtime.value / partime.value} ms")
  }

}
