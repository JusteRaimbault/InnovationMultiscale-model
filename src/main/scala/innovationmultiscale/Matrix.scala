package innovationmultiscale

import org.apache.commons.math3.linear
import org.apache.commons.math3.linear.{LUDecomposition, MatrixUtils}

import scala.util.Random


/**
 * Generic methods for double matrices
 *   rq: could make generic in Numeric type?
 */
sealed trait Matrix {

  /**
   * rq: in immutable terms, setting is not efficient (should clone the underlying matrix?)
   * @param i row
   * @param j column
   * @param v value
   * @return cloned matrix with value set
   */
  def set(i: Int, j: Int, v: Double): Matrix

  /**
   * Mutable set
   * @param i row
   * @param j column
   * @param v value
   */
  def setM(i: Int, j: Int, v: Double): Unit

  /**
   * get element
   * @param i row
   * @param j column
   * @return value
   */
  def get(i: Int, j: Int): Double

  /**
   * get submatrix
   * @param i starting row
   * @param j starting column
   * @param nrows number of rows
   * @param ncols number of columns
   * @return submatrix
   */
  def getSubmat(i: Int, j: Int, nrows: Int, ncols: Int): Matrix

  def getRow(i: Int): Matrix = getSubmat(i,0,1,ncols)
  def getCol(j: Int): Matrix = getSubmat(0,j,nrows,1)

  /**
   * unoptimal mutable submat set
   * @param i starting row
   * @param j starting col
   * @param a values
   */
  def setMSubmat(i: Int, j: Int, a: Array[Array[Double]]): Unit = {
    a.zipWithIndex.foreach{case (row,k) =>
      row.zipWithIndex.foreach{case (v,l) =>
        if(i+k<nrows&&j+l<ncols) setM(i+k,j+l,v)
      }
    }
  }

  /**
   * map element by element
   * @param f function to map
   * @return
   */
  def map(f: Double => Double): Matrix

  // !! mutable
  def values: Array[Array[Double]]

  def flatValues: Array[Double]

  def nrows: Int
  def ncols: Int

  //basic ring operations (R style for the notation)
  def %*%(m: Matrix): Matrix

  // scalar operations
  def +(d: Double): Matrix = map(_+d)
  def -(d: Double): Matrix = map(_-d)
  def *(d: Double): Matrix = map(_*d)

  def +(m: Matrix): Matrix
  def -(m: Matrix): Matrix
  def *(m: Matrix): Matrix
  def ^(m: Matrix): Matrix

  def transpose: Matrix
  def inverse: Matrix // should use pseudo inverse or restrict?
  def determinant: Double

  def sum: Double
  def mean: Double = sum/(nrows*ncols)
  def min: Double
  def max: Double

  def rowSum: Array[Double]
  def colSum: Array[Double]

}

/**
 * matrix utilities
 *
 *  - test spark matrices https://spark.apache.org/docs/2.1.2/api/java/org/apache/spark/mllib/linalg/Matrix.html?
 *
 * mutable / immutable matrices?
 * https://medium.com/@eob/how-you-might-create-a-scala-matrix-library-in-a-functional-programming-style-760f8bf6ee6
 *
 * Also Jama for matrices Jama.Matrix
 *
 */
object Matrix {

  sealed trait MatrixImplementation
  case class Dense(denseImpl: DenseMatrix.DenseMatrixImplementation) extends MatrixImplementation

  val defaultImplementation: MatrixImplementation = Dense(DenseMatrix.Real())
  //private implicit val locDefaultImpl: MatrixImplementation = defaultImplementation

  /**
   * could make this implicit also?
   * @param m matrix
   */
  def getImplementation(m: Matrix): MatrixImplementation = m match {
    case _: RealMatrix => Matrix.Dense(DenseMatrix.Real())
  }

  /**
   *
   * Constructs given implicit implementations
   *
   * @param a array of array
   * @return
   */
  def apply(a: Array[Array[Double]])(implicit matrixImpl: MatrixImplementation): Matrix = matrixImpl match {
    case Dense(dmImpl) => DenseMatrix(a)(dmImpl)
  }


  /**
   * column or row constructor
   * @param a array
   * @param row row or column matrix
   * @param matrixImpl implementation
   * @return matrix
   */
  def apply(a: Array[Double], row: Boolean=true)(implicit matrixImpl: MatrixImplementation): Matrix = if(row) apply(Array(a)) else apply(a.map(Array(_)))

  /*
    * Row bind two matrices
    *   !!! rewrite specific for sparse mats
    */
  //def rbind(a1: Matrix, a2: Matrix): Matrix = Matrix(a1.values++a2.values)

  /*
    * Row bind several matrices
    */
  //def rbind(a: Array[Matrix]): Matrix = a.reduce(rbind)

  /*
    * Column bind two matrices
    */
  //def cbind(a1: Matrix, a2: Matrix): Matrix = Matrix((a1.values.transpose++a2.values.transpose).transpose)

  /*
    * Column bind several matrices
    */
  //def cbind(a: Array[Matrix]): Matrix = a.reduce(cbind)

  def msum: (Matrix,Matrix)=>Matrix = {case m: (Matrix,Matrix) => m._1+m._2}


}


/**
 * Dense matrix
 */
sealed trait DenseMatrix extends Matrix {
  override def rowSum: Array[Double] = values.map{_.sum}
  override def colSum: Array[Double] = transpose.values.map{_.sum}
  override def sum: Double = values.flatten.sum
  override def min: Double = values.flatten.min
  override def max: Double = values.flatten.max
  override def flatValues: Array[Double] = values.flatten
}

object DenseMatrix {

  sealed trait DenseMatrixImplementation //extends MatrixImplementation
  case class Real() extends DenseMatrixImplementation
  case class DenseBreeze() extends DenseMatrixImplementation

  private implicit val defaultDenseMatrixImplementation: DenseMatrixImplementation = Real()


  /**
   * Construct given an implicit implementation
   * @param a values
   * @param dmImpl implicit implementation
   * @return
   */
  def apply(a: Array[Array[Double]])(implicit dmImpl: DenseMatrixImplementation): DenseMatrix = dmImpl match {
    case _: Real => RealMatrix(a)
    case _ => RealMatrix(a)
  }

  /**
   * zeros
   * @param n rows
   * @param p columns
   * @return
   */
  def zeros(n: Int, p: Int): DenseMatrix = constant(n,p,0.0)

  def ones(n: Int, p: Int): DenseMatrix = constant(n,p,1.0)

  def constant(n: Int, p: Int, v: Double): DenseMatrix = DenseMatrix(Array.fill(n)(Array.fill(p)(v)))

  def diagonal(a: Array[Double]): DenseMatrix = DenseMatrix(Array.tabulate(a.length,a.length){case (i,j) => if(i==j)a(i) else 0.0})

  /**
   * random dense matrix
   * @param n rows
   * @param p columns
   * @param density density
   * @param rng implicit random
   * @return
   */
  def randomDenseMatrix(n: Int, p: Int, density: Double)(implicit rng: Random): DenseMatrix = {
    //val m = zeros(n,p)
    //val inds: Seq[(Int,Int)] = Stochastic.sampleWithoutReplacement[(Int,Int)](for {i <- 0 until n;j <- 0 until p} yield (i,j), (n*p*density).toInt)
    //inds.map{case (i,j) => (i,j,rng.nextDouble)}
    // approximate density by drawing at each step
    val values = (for {_ <- 0 until n
                       _ <- 0 until p
                       r = rng.nextDouble()
                       } yield  if (r < density) 0.0 else rng.nextDouble()).toArray.grouped(p).toArray
    DenseMatrix(values)
  }


}

/**
 * Apache commons real matrix as implementation of dense matrix
 * @param m apache RealMatrix
 */
case class RealMatrix(m: linear.RealMatrix) extends DenseMatrix {

  // the object clone does not clone internal references ?
  override def clone: RealMatrix = RealMatrix(m.getData.clone.map(_.clone))

  override def nrows: Int = m.getRowDimension

  override def ncols: Int = m.getColumnDimension

  override def set(i: Int, j: Int, v: Double): Matrix = {
    val d = clone.m
    d.setEntry(i, j, v)
    RealMatrix(d)
  }

  /**
   * Mutable set
   *
   * @param i row
   * @param j column
   * @param v value
   */
  def setM(i: Int, j: Int, v: Double): Unit = m.setEntry(i, j, v)

  def setSubmatM(i: Int, j: Int, d: Array[Array[Double]]): Unit = m.setSubMatrix(d, i, j)

  def setDiagM(d: Double): Unit = (0 until math.min(nrows, ncols)).foreach(i => setM(i, i, d))

  def setDiagM(a: Array[Double]): Unit = a.zipWithIndex.foreach { case (d, i) => setM(i, i, d) }


  /**
   * !!! using getSubmatrix should give issues with mutable -> necessary to copy (~ unoptimal)
   * @param i starting row
   * @param j starting column
   * @param nrows number of rows
   * @param ncols number of columns
   * @return submatrix
   */
  override def getSubmat(i: Int, j: Int, nrows: Int, ncols: Int): Matrix = {
    val dest: Array[Array[Double]] = Array.fill(nrows,ncols)(0.0)
    m.copySubMatrix((i until i + nrows).toArray, (j until j + ncols).toArray,dest)
    RealMatrix(dest)
  }

  override def get(i: Int, j: Int): Double = m.getEntry(i,j)

  /**
   * for map, go through each element anyway, cloning is less an issue
   * @param f function
   * @return
   */
  override def map(f: Double => Double): Matrix = {
    val d = m.getData.clone.map(_.clone)
    RealMatrix(d.map(_.map(f)))
  }

  override def values: Array[Array[Double]] = m.getData

  def dispatchOp(op: RealMatrix => RealMatrix): Matrix => Matrix = {
    m2: Matrix => m2 match {
      case m2: RealMatrix => op(m2)
      case _ => throw new UnsupportedOperationException("Matrix implementations combination not supported")
    }
  }

  //override def %+%(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.add(m2.m))}(m2)

  override def %*%(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.multiply(m2.m))}(m2)
  override def +(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 + v2}})}(m2)
  override def -(m2: Matrix): Matrix = dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 - v2}})}(m2)
  override def *(m2: Matrix): Matrix =  dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => v1 * v2}})}(m2)
  override def ^(m2: Matrix): Matrix =  dispatchOp{m2=>RealMatrix(m.getData.zip(m2.m.getData).map{case (row1,row2) => row1.zip(row2).map{case (v1,v2) => math.pow(v1,v2)}})}(m2)

  override def transpose: Matrix = RealMatrix(m.transpose())
  override def determinant: Double = new LUDecomposition(m).getDeterminant
  override def inverse: Matrix = RealMatrix(MatrixUtils.inverse(m))

  override def toString: String = s"Dense real matrix of size ${nrows}x$ncols - internal: ${m.getRowDimension}x${m.getColumnDimension}"

}

object RealMatrix {

  def apply(a: Array[Array[Double]]): RealMatrix = RealMatrix(linear.MatrixUtils.createRealMatrix(a))
  def apply(a: Array[Double],row: Boolean=true): RealMatrix = if(row) apply(Array(a)) else apply(a.map(Array(_)))

  def zeros(n: Int, p: Int): RealMatrix = constant(n,p,0.0)
  def ones(n: Int, p: Int): RealMatrix = constant(n,p,1.0)
  def constant(n: Int, p: Int, v: Double): RealMatrix = RealMatrix(Array.fill(n)(Array.fill(p)(v)))


}

