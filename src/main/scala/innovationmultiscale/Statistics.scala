package innovationmultiscale

import org.apache.commons.math3.stat.regression.SimpleRegression

import scala.math.{Ordering, log}

object Statistics {

  /**
   * Distribute a variable following a rank size
   * @param size number of elements
   * @param alpha slope
   * @param pmax max value
   * @return
   */
  def rankSizeDistribution(size: Int,alpha: Double, pmax: Double): Vector[Double] =
    (1 to size by 1).map{i => pmax*math.pow(1.0/i.toDouble,alpha)}.toVector


  def slope(values: Array[Double]): (Double,Double) = {
    def distribution: Array[Double] = values.filter(_ > 0).sorted(Ordering.Double.TotalOrdering.reverse)
    def distributionLog: Array[Array[Double]] = distribution.zipWithIndex.map { case (q, i) => Array(log(i + 1), log(q)) }
    val simpleRegression = new SimpleRegression(true)
    simpleRegression.addData(distributionLog)
    (simpleRegression.getSlope, simpleRegression.getRSquare)
  }

}
