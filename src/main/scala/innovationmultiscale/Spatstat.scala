package innovationmultiscale

import org.apache.commons.math3.linear.MatrixUtils

object Spatstat {

  /**
   * Euclidian distance matrix
   *  rq: unoptimal, + already coded with great circle dist in geotools
   * @param pi points
   * @return
   *
   */
  def euclidianDistanceMatrix(pi: Array[(Double,Double)]): Array[Array[Double]] = {
    val n = pi.length
    val xcoords = MatrixUtils.createRealMatrix(Array.fill(n)(pi.map(_._1)))
    val ycoords = MatrixUtils.createRealMatrix(Array.fill(n)(pi.map(_._2)))
    MatrixUtils.createRealMatrix(
      xcoords.subtract(xcoords.transpose()).getData.map(_.map( x => x*x))
    ).add(
      MatrixUtils.createRealMatrix(
        ycoords.subtract(ycoords.transpose()).getData.map(_.map( x => x*x))
      )
    ).getData.map(_.map(math.sqrt))
  }

}
