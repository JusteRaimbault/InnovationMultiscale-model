package innovationmultiscale

import innovationmultiscale.Result.MesoResult

case class Result(
                 states: Seq[State]
                 ){

  def populations: Matrix = {
    val p = states.map(_.macroState).last.populations
    p.getSubmat(0,0,p.nrows,p.ncols-1)
  }
  def innovationShares: Seq[Matrix] = {
    val i = states.map(_.macroState).last.innovations
    i.map(m => m.getSubmat(0,0,m.nrows,m.ncols-1))
  }
  def innovationUtilities: Seq[Double] = states.map(_.macroState).last.utilities


  def macroUtilities: Seq[Double] = {
    //println(populations.colSum.toSeq)
    //println(innovationUtilities)
    //println(innovationShares.map(_.colSum))
    val normPop = populations%*%DenseMatrix.diagonal(populations.colSum.map(1/_))
    val perCityInnovTimeavg: Seq[Seq[Double]] = innovationShares.zip(innovationUtilities).map{case (m,u)=> (normPop*m*(u/m.ncols)).colSum.toSeq}
    perCityInnovTimeavg.transpose.map(c => c.sum / c.length.toDouble)
  }

  def averageMacroUtility: Double = macroUtilities.sum

  def macroDiversities: Seq[Double] = {
    def arraySum(a1: Array[Double], a2: Array[Double]): Array[Double] = a1.zip(a2).map{case (x1,x2)=> x1+x2}
    val normPop =populations%*%DenseMatrix.diagonal(populations.colSum.map(1/_))
    val perCityInnovTimeDiversity = innovationShares.map(m => (normPop*m).map(x => x*x).colSum.map(1 - _).toSeq)
      //.reduceLeft(arraySum).map(1 - _).toSeq
    perCityInnovTimeDiversity.transpose.map(c => c.sum / c.length.toDouble)
  }

  def averageMacroDiversity: Double = {
    val divs = macroDiversities
    divs.sum / divs.length.toDouble
  }

  def averageMacroInnovation: Double = {
    innovationUtilities.length.toDouble/(populations.nrows.toDouble*populations.ncols.toDouble)
  }

  def averageMesoProductDiversity: Double = {
    val alldiversities: Seq[Double] = states.flatMap(_.mesoStates.map(MesoResult(_).productDiversities.last))
    alldiversities.sum / alldiversities.size.toDouble
  }

  def averageMesoBestFitness: Double = {
    val allbestfitnesses: Seq[Double] = states.flatMap(_.mesoStates.map(MesoResult(_).bestFitnesses.last))
    allbestfitnesses.sum / allbestfitnesses.size.toDouble
  }

  /**
   * meso ts at macro time steps, for all indics and all cities
   * @return
   */
  def mesoTimeSeries: Seq[Seq[Double]] = {
    val divs = states.map(_.mesoStates.map(MesoResult(_).productDiversities.last))
    //println(s"${divs.length}x${divs.head.length}")
    divs.transpose++
    states.map(_.mesoStates.map(MesoResult(_).bestFitnesses.last)).transpose
  }

}


object Result {



  case class MesoResult(states: Seq[MesoState]){

    def fitnesses: Array[Array[Double]] = states.map(_.firms.map(_.currentFitness).toArray).toArray

    def bestFitnesses: Array[Double] = states.map(_.firms.map(_.currentFitness).max).toArray

    def worseFitnesses: Array[Double] = states.map(_.firms.map(_.currentFitness).min).toArray

    def fitnessDifferences: Array[Double] = states.map{s => math.abs(s.firms.map(_.currentFitness).max - s.firms.map(_.currentFitness).min)/math.abs(s.firms.map(_.currentFitness).max)}.toArray

    def interactionIntensity: Double = states.drop(1).map(_.interactionIntensity).sum/states.size.toDouble

    def productDiversities: Array[Double] = states.map(MesoResult.productDiversity).toArray

    def fitnessEntropies: Array[Double] = states.map(MesoResult.fitnessEntropy).toArray

  }

  object MesoResult {
    def productDiversity(state: MesoState): Double = {
      val products = state.firms.map(_.currentProduct)
      val proximities = products.flatMap{pi =>
        val ni = Statistics.norm(pi)
        products.map{pj =>
          val nj = Statistics.norm(pj)
          (1 - (pi.zip(pj).map{case (pik,pjk) => pik*pjk}.sum/(ni*nj)))/2.0
        }
      }
      proximities.sum/proximities.size.toDouble
    }

    def fitnessEntropy(state: MesoState): Double = {
      val fitnesses: Seq[Double] = state.firms.map(_.currentFitness)
      val (mi, ma) = (fitnesses.min, fitnesses.max)
      //val normfitnesses = fitnesses.map(f => (f - mi)/(ma - mi))
      val normfitnesses = if (mi < 0.0) fitnesses.map(f => f - mi) else fitnesses // entropy % 0 (normalisation ma/mi gives always similar entropy)
      val s = normfitnesses.sum
      val p = normfitnesses.map(_ / s)
      val n = p.size.toDouble
      -1.0*p.map(pi => if(pi==0.0) 0.0 else pi*math.log(pi)).sum/(n*math.log(n))
    }
  }

}
