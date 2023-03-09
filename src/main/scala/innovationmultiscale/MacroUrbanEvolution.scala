package innovationmultiscale

import innovationmultiscale.Matrix.MatrixImplementation

import scala.util.Random

/**
 * synthetic only -> no pop matrix
 */
case class MacroUrbanEvolution(syntheticCities: Int,
                               syntheticHierarchy: Double,
                               syntheticMaxPop: Double,
                               distanceMatrix: Matrix,
                               finalTime: Int,
                               rng : Random,
                               growthRate: Double,
                               innovationWeight: Double,
                               gravityDecay: Double,
                               innovationDecay: Double,
                               // fixed endog
                               //newInnovation: (Seq[Double],Seq[Double],Seq[Seq[Double]]) => (Boolean,Seq[Double],Seq[Seq[Double]]),
                               initialInnovationUtility: Double)


object MacroUrbanEvolution {


  sealed trait InnovationUtilityDistribution
  case class InnovationUtilityNormalDistribution() extends InnovationUtilityDistribution
  case class InnovationUtilityLogNormalDistribution() extends InnovationUtilityDistribution


  def apply(
             syntheticCities: Int,
             syntheticHierarchy: Double,
             syntheticMaxPop: Double,
             finalTime: Int,
             seed: Int,
             growthRate: Double,
             innovationWeight: Double,
             gravityDecay: Double,
             innovationDecay: Double,
             mutationRate: Double,
             newInnovationHierarchy: Double,
             earlyAdoptersRate: Double,
             utilityStd: Double,
             utilityDistribution: String
           ): MacroUrbanEvolution = {
    implicit val m: MatrixImplementation = Matrix.defaultImplementation
    implicit val rng: Random = new Random
    rng.setSeed(seed.toLong)

    // could be initialised in init state
    val dmat = Matrix(Spatstat.euclidianDistanceMatrix(RandomPointsGenerator(syntheticCities).generatePoints.toArray))
    val initialPopulations = Statistics.rankSizeDistribution(syntheticCities, syntheticHierarchy, syntheticMaxPop)
    val populationMatrix = DenseMatrix.zeros(syntheticCities,finalTime+1)
    populationMatrix.setMSubmat(0,0,Array(initialPopulations.toArray).transpose)


    //val distrib = utilityDistribution match {
    //  case "normal" => InnovationUtilityNormalDistribution()
    //  case "log-normal" =>
    //    assert(utilityStd>math.exp(-0.5),"For a log-normal distribution with mu=0, std must be > to 0.6")
    //   InnovationUtilityLogNormalDistribution()
    //}

    MacroUrbanEvolution(syntheticCities, syntheticHierarchy, syntheticMaxPop, dmat,finalTime,rng,growthRate,innovationWeight,gravityDecay,innovationDecay,
      //mutationInnovation(_,_,_, mutationRate, newInnovationHierarchy, earlyAdoptersRate, utilityStd, distrib),
      1.0
    )

  }


}