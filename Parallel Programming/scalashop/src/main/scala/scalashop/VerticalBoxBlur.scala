package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    for(x <- from until end){
      for(y <- 0 until src.height){
        dst(x,y) = boxBlurKernel(src, x, y, radius)
      }
    }

  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    /*
    val numTasks : List[(Int,Int)] = {
      val rangeList = (0 until src.width-1).toList
      if (rangeList.max == src.width -1) rangeList.zip(rangeList.tail)
      else (rangeList:+ (src.width -1)).zip((rangeList:+ (src.width -1)).tail)
    }
    numTasks.map{case (from,end) => task(blur(src, dst, from, end, radius))}.foreach(_.join())
    */
    val colsPerTaks:Int = Math.max(src.width / numTasks,1)
    val startPoints = Range(0, src.width) by colsPerTaks

    val tasks = startPoints.map(t => {
      task {
        blur(src, dst, t, t + colsPerTaks, radius)
      }
    })

    tasks.map(t => t.join())
  }
}
