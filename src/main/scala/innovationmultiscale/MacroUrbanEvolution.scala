package innovationmultiscale

case class MacroUrbanEvolution()


object MacroUrbanEvolution {

  /**
   * Synthetic setup for mutation model
   * @param syntheticCities number of cities
   * @param syntheticHierarchy initial hierarchy
   * @param syntheticMaxPop initial max population
   * @param finalTime number of time steps
   * @param seed seed
   * @param growthRate Gibrat growth rate
   * @param innovationWeight innovation growth rate
   * @param gravityDecay gravity decay
   * @param innovationDecay innovation diffusion decay
   * @param mutationRate mutation rate
   * @param newInnovationHierarchy new innovation hierarchy
   * @param earlyAdoptersRate early adoption proportion
   * @param utilityStd utility standard dviation
   * @param utilityDistribution type of distribution: "normal" or "log-normal"
   * @return
   */
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

    val dmat = Matrix(Spatstat.euclidianDistanceMatrix(RandomPointsGenerator(syntheticCities).generatePoints.toArray))
    val initialPopulations = Statistics.rankSizeDistribution(syntheticCities, syntheticHierarchy, syntheticMaxPop)
    val populationMatrix = DenseMatrix.zeros(syntheticCities,finalTime+1)
    populationMatrix.setMSubmat(0,0,Array(initialPopulations.toArray).transpose)
    val dates: Array[Double] = (0 to finalTime).toArray.map{_.toDouble}
    val distrib = utilityDistribution match {
      case "normal" => InnovationUtilityNormalDistribution()
      case "log-normal" =>
        assert(utilityStd>math.exp(-0.5),"For a log-normal distribution with mu=0, std must be > to 0.6")
        InnovationUtilityLogNormalDistribution()

    }

    UrbanEvolution(populationMatrix,dmat,dates,rng,growthRate,innovationWeight,gravityDecay,innovationDecay,
      mutationInnovation(_,_,_, mutationRate, newInnovationHierarchy, earlyAdoptersRate, utilityStd, distrib),
      1.0
    )

  }


}