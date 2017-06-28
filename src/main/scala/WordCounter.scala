package parallel
import org.scalameter.{Key, Warmer, config}

import scala.io.Source


object WordCounter {

  val monoid = new Monoid[(Boolean, Int, Boolean)] {
    def op(x: (Boolean, Int, Boolean) , y: (Boolean, Int, Boolean) ) = {
      (x._1, x._2 + (if (x._3 && !y._1) 1 else 0) + y._2, y._3)
    }
    def zero = (false, 0, false)
  }

  def mapChar(c: Char): (Boolean, Int, Boolean) = {
    (c != ' ', 0, c != ' ')
  }

  def wordCountInLine(line: String): Int ={
    val res = parallelFoldMap.foldMapSegment(line, 0, line.length(), monoid)(mapChar)
    res._2 + (if(res._3) 1 else 0)
  }

  def wordCountInLinePar(line: String, thresh: Int): Int ={
    val res = parallelFoldMap.foldMapPar(line, 0, line.length(), monoid)(mapChar)(thresh)
    res._2 + (if(res._3) 1 else 0)
  }

  def wordCount(path: String): Int ={
    val bufferedSource = Source.fromFile(path)

    var res = 0
    for(line <- bufferedSource.getLines) {
      res += wordCountInLine(line)
    }
    bufferedSource.close

    res
  }

  def wordCountPar(path: String, thresh: Int): Int ={
    val bufferedSource = Source.fromFile(path)

    var res = 0
    for(line <- bufferedSource.getLines()) {
     res += wordCountInLinePar(line, thresh)
    }

    bufferedSource.close

    res
  }

  def main(args: Array[String]): Unit = {
    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true) withWarmer (new Warmer.Default)


    val seqtime = standardConfig measure {
      wordCount("file.txt")
    }

    val partime = standardConfig measure {
      wordCountPar("file.txt", 600)
    }

    println(s"sequential time $seqtime ms")
    println(s"parallel time $partime ms")

    println(s"speedup: ${seqtime.value / partime.value} ms")
  }

}

trait Monoid [A] {
  def op(x : A, y : A): A
  def zero : A
}


package object parallelFoldMap{
  def foldMapPar [A, B] ( xs : IndexedSeq [A] ,
                          from : Int , to : Int ,
                          m: Monoid [B])
                        (f : A => B)
                        (implicit theresholdSize : Int ) : B =
    if(to - from <= theresholdSize)
      foldMapSegment(xs, from, to, m)(f)
    else {
      val middle = from + (to - from)/2
      val (l, r) = parallel(
        foldMapPar (xs, from, middle, m)(f)
                      (theresholdSize),
        foldMapPar (xs, middle, to, m)(f)
                      (theresholdSize))
      m.op(l, r)
    }

  def foldMapSegment [A, B] ( xs : IndexedSeq [A] ,
                              from : Int , to : Int ,
                              m: Monoid [B])
  (f : A => B) : B = {
    var res = f(xs(from))
    var index = from + 1
    while(index < to) {
      res = m.op(res, f(xs(index)))
      index = index + 1
    }
    res
  }
}
