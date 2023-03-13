package innovationmultiscale

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import innovationmultiscale.Matrix.MatrixImplementation

/**
 * Modifications from the original model:
 *  - synthetic systems only -> no pop matrix
 *  - only log-normal distrib
 *  - innov emergence depends on the mesoscale -> param equivalent to mutationRate and newInnovationHierarchy are inter-scale parameters
 */
case class MacroUrbanEvolution(
                                syntheticCities: Int,
                                syntheticHierarchy: Double,
                                syntheticMaxPop: Double,
                                finalTime: Int,
                                growthRate: Double,
                                innovationWeight: Double,
                                gravityDecay: Double,
                                innovationDecay: Double,
                                earlyAdoptersRate: Double,
                                utilityStd: Double,
                                initialInnovationUtility: Double
                              )


object MacroUrbanEvolution {

  def setup(model: MacroUrbanEvolution)(implicit rng: Random): MacroState = {
    implicit val m: MatrixImplementation = Matrix.defaultImplementation
    import model._
    val dmat = Matrix(Spatstat.euclidianDistanceMatrix(RandomPointsGenerator(syntheticCities).generatePoints.toArray))
    val initialPopulations = Statistics.rankSizeDistribution(syntheticCities, syntheticHierarchy, syntheticMaxPop)
    val populationMatrix = DenseMatrix.zeros(syntheticCities,finalTime+1)
    populationMatrix.setMSubmat(0,0,Array(initialPopulations.toArray).transpose)

    val innovationUtilities: ArrayBuffer[Double] = new ArrayBuffer[Double]
    innovationUtilities.append(model.initialInnovationUtility)
    val innovationProportions: ArrayBuffer[Matrix] = new ArrayBuffer[Matrix]
    val archaicTechno =  DenseMatrix.zeros(populationMatrix.nrows,populationMatrix.ncols)
    archaicTechno.setMSubmat(0, 0, Array.fill(populationMatrix.nrows)(Array(1.0)))
    innovationProportions.append(archaicTechno)

    val gravityDistanceWeights = dmat.map { d => Math.exp(-d / gravityDecay) }
    val potsgravity = gravityPotentials(innovationProportions.toSeq, Array(1.0), populationMatrix.getCol(0).flatValues, gravityDistanceWeights, 0)

    MacroState(0, populationMatrix, innovationProportions.toSeq, innovationUtilities.toSeq, dmat, potsgravity)
  }


  def gravityPotentials(diffusedInnovs: Seq[Matrix], macroAdoptionLevels: Array[Double], currentPopulations: Array[Double], gravityDistanceWeights: Matrix, time: Int): Matrix = {
    val technoFactor: Array[Double] = diffusedInnovs.zip(macroAdoptionLevels).map{
      case(m,phi)=>
        m.getCol(time).flatValues.map{math.pow(_,phi)}
    }.toArray.foldLeft(Array.fill(diffusedInnovs.head.nrows)(1.0)){case(a1,a2)=>a1.zip(a2).map{case(d1,d2)=> d1*d2}}

    val totalpop = currentPopulations.sum
    val diagpops = DenseMatrix.diagonal(currentPopulations)*(1 / totalpop)
    diagpops %*% DenseMatrix.diagonal(technoFactor) %*% gravityDistanceWeights %*% diagpops
  }

  def macroStep(model: MacroUrbanEvolution, state: MacroState): MacroState = {

    import model._
    val gravityDistanceWeights = state.distanceMatrix.map { d => Math.exp(-d / gravityDecay) }
    val innovationDistanceWeights = state.distanceMatrix.map { d => Math.exp(-d / innovationDecay) }

    val n = state.populations.nrows
    val p = state.populations.ncols
    val delta_t = 1.0
    val totalpop = state.populations.sum
    val currentPopulations = state.populations.flatValues

    val tmplevel: Array[Array[Double]] = state.innovations.zip(state.utilities).map{
      case (m,utility)=>
        (Matrix(Array(
          m.getCol(state.time).flatValues.zip(currentPopulations).map{
            case(previousshare,citypop)=> math.pow(previousshare*citypop/totalpop,1/utility)}))(Matrix.defaultImplementation)%*%
          innovationDistanceWeights).flatValues
    }.toArray

    val cumtmp: Array[Double] = tmplevel.foldLeft(Array.fill(n)(0.0)){case (a1,a2)=>a1.zip(a2).map{case(d1,d2)=>d1+d2}}
    val deltaci: Array[Array[Double]] = tmplevel.map{_.zip(cumtmp).map{case (d1,d2)=>d1 / d2}}
    //utils.log("deltaci = "+deltaci.map(_.toSeq).toSeq)
    val diffusedInnovs = state.innovations.zip(deltaci).map{
      case (innovmat,cityprops)=>
        val r: Matrix = innovmat.asInstanceOf[RealMatrix].clone
        r.setMSubmat(0,state.time + 1, cityprops.map(Array(_)))
        r
    }
    // compute macro adoption levels
    val macroAdoptionLevels: Array[Double] = diffusedInnovs.map{
      _.getCol(state.time + 1).flatValues.zip(currentPopulations).map{case(w,d)=>w*d}.sum
    }.map{_ / totalpop}.toArray

    val potsgravity = gravityPotentials(diffusedInnovs, macroAdoptionLevels, currentPopulations, gravityDistanceWeights, state.time + 1)
    val meanpotgravity = potsgravity.sum / (n * n)


    val prevpop = Matrix(currentPopulations.map(Array(_)))(Matrix.defaultImplementation)

    val newPopulations = (prevpop +
      (prevpop *
        (
          ((potsgravity %*% DenseMatrix.ones(n,1) * (innovationWeight / (n * meanpotgravity)))
            + DenseMatrix.constant(n, 1, growthRate)
            ) * delta_t
          )
        )
      ).flatValues

    val currentInnovProps: Seq[Seq[Double]] = diffusedInnovs.map(_.getCol(state.time+1).flatValues.toSeq)

    // FIXME this should come from the meso cycle
    //val potentialInnovation: (Boolean, Seq[Double], Seq[Seq[Double]]) = model.newInnovation(newPopulations.toSeq, state.utilities.toSeq, currentInnovProps)
    val potentialInnovation: (Boolean, Seq[Double], Seq[Seq[Double]]) = (false, Seq.empty[Double], Seq.empty[Seq[Double]])

    val res = if (potentialInnovation._1){
      val newutilities = state.utilities ++ potentialInnovation._2
      val newShares = potentialInnovation._3
      // update old innovations (note that remaining of potentialInnovation._3 is ignored by the zip)
      val newInnovProp = state.innovations.toArray.zip(newShares).map{
        case (m,newprop) =>
          //utils.log("   update prop old = "+newprop)
          val r = m.asInstanceOf[RealMatrix].clone
          r.setMSubmat(0,state.time + 1,Array(newprop.toArray).transpose)
          r
      }
      // add new innovation matrices
      val addInnovProp = newShares.takeRight(newShares.length-newInnovProp.length).map{s =>
        //utils.log("   update prop new = "+s.toSeq)
        val newInnovMat = DenseMatrix.zeros(n,p)
        newInnovMat.setMSubmat(0,state.time + 1,Array(s.toArray).transpose)
        newInnovMat
      }

      state.copy(
        time = state.time + 1,
        populations = Matrix(newPopulations, row = false)(Matrix.defaultImplementation),
        innovations = newInnovProp.toSeq++addInnovProp,
        utilities = newutilities,
        flows = potsgravity
      )

    } else state.copy(
      time = state.time + 1,
      populations = Matrix(newPopulations, row = false)(Matrix.defaultImplementation),
      innovations = diffusedInnovs,
      flows = potsgravity
    )
    res
  }



}