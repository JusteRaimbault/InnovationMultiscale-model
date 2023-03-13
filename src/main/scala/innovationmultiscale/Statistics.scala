package innovationmultiscale

import infodynamics.measures.continuous.kraskov.MutualInfoCalculatorMultiVariateKraskov1

import org.apache.commons.math3.stat.regression.SimpleRegression
import org.apache.commons.math3.random.RandomDataGenerator

import scala.jdk.CollectionConverters.IterableHasAsJava
import scala.math.{Ordering, log}
import scala.util.Random

object Statistics {

  def norm(s: Seq[Double]): Double = math.sqrt(s.map{si => si*si}.sum)

  /**
   * Distribute a variable following a rank size
   * @param size number of elements
   * @param alpha slope
   * @param pmax max value
   * @return
   */
  def rankSizeDistribution(size: Int,alpha: Double, pmax: Double): Vector[Double] =
    (1 to size by 1).map{i => pmax*math.pow(1.0/i.toDouble,alpha)}.toVector

  def moment(x: Array[Double],order: Int = 1,weighting : Array[Double]=Array.empty,filter: Double => Boolean = _ => true): Double = {
    val w: Array[Double] = x.zip(weighting).filter{case (xx,_)=>filter(xx)}.map{case (_,ww)=> ww} match {
      case a if a.length==0 => Array.fill(x.length){1.0/x.length}
      case a if a.sum != 1.0 =>
        val s = a.sum
        a.map{_/s}
      case a => a
    }
    x.filter(filter).zip(w).map{case (xx,ww) => ww*math.pow(xx,order.toDouble)}.sum
  }

  def std(x: Array[Double]): Double = {
    val ex = moment(x)
    math.sqrt(moment(x,2) - ex*ex)
  }

  def slope(values: Array[Double]): (Double,Double) = {
    def distribution: Array[Double] = values.filter(_ > 0).sorted(Ordering.Double.TotalOrdering.reverse)
    def distributionLog: Array[Array[Double]] = distribution.zipWithIndex.map { case (q, i) => Array(log(i + 1), log(q)) }
    val simpleRegression = new SimpleRegression(true)
    simpleRegression.addData(distributionLog)
    (simpleRegression.getSlope, simpleRegression.getRSquare)
  }

  /**
   * ex https://github.com/jlizier/jidt/wiki/SimpleJavaExamples#example-6---late-binding-mutual-info-calculator
   * @param X
   * @param Y
   * @return
   */
  def mutualInformation(X: Array[Double], Y: Array[Double]): Double = {
    val calculator = new MutualInfoCalculatorMultiVariateKraskov1()
    calculator.setObservations(X,Y)
    calculator.computeAverageLocalOfObservations()
  }

  /**
   * psi > 0 for causal emergence
   * @param X
   * @param V
   * @param tau
   * @return
   */
  def psi(X: Array[Array[Double]], V: Array[Double], tau: Int = 1): Double = {
    val v = V.dropRight(tau)
    val vlagged = V.drop(tau)
    mutualInformation(v, vlagged) - X.map(mutualInformation(_,vlagged)).sum
  }

  def measureBootstrapped(f: (Array[Array[Double]], Array[Double], Int) => Double, X: Array[Array[Double]], V: Array[Double], tau: Int = 1, nbootstraps: Int = 100)(implicit rng: Random): (Double, Double) = {
    val res = f(X,V,tau)
    val sampler = new RandomDataGenerator()
    sampler.reSeed(rng.nextLong())
    val sigma = std((1 to nbootstraps).map{_ =>
      f(X.map(xj => sampler.nextSample(xj.toSeq.asJavaCollection, xj.length).asInstanceOf[Array[Double]]),sampler.nextSample(V.toSeq.asJavaCollection, V.length).asInstanceOf[Array[Double]],tau)
    }.toArray)
    (res, sigma)
  }

  def psiWithStd(X: Array[Array[Double]], V: Array[Double], tau: Int = 1, nbootstraps: Int = 100)(implicit rng: Random): (Double, Double) =
    measureBootstrapped(psi, X, V, tau, nbootstraps)

  /**
   * delta > 0 for downward causation
   * @param X
   * @param V
   * @param tau
   * @return
   */
  def delta(X: Array[Array[Double]], V: Array[Double], tau: Int = 1): Double = {
    val v = V.dropRight(tau)
    val x = X.map(_.dropRight(tau))
    val xlagged = X.map(_.drop(tau))
    xlagged.map { xj =>
      mutualInformation(v, xj) - x.map(mutualInformation(_, xj)).sum
    }.max
  }

  def deltaWithStd(X: Array[Array[Double]], V: Array[Double], tau: Int = 1, nbootstraps: Int = 100)(implicit rng: Random): (Double, Double)=
    measureBootstrapped(delta, X, V, tau, nbootstraps)

  /**
   * gamma = 0 and psi > 0 for causal decoupling
   * @param X
   * @param V
   * @param tau
   * @return
   */
  def gamma(X: Array[Array[Double]], V: Array[Double], tau: Int = 1): Double = {
    val v = V.dropRight(tau)
    X.map(xj => mutualInformation(v, xj.drop(tau))).max
  }

  def gammaWithStd(X: Array[Array[Double]], V: Array[Double], tau: Int = 1, nbootstraps: Int = 100)(implicit rng: Random): (Double, Double)=
    measureBootstrapped(gamma, X, V, tau, nbootstraps)

}
