package innovationmultiscale

import scala.util.Random

case class InnovationMultiscale(
                               seed: Long,
                               finalTime: Int,
                               macroSyntheticCities: Int,
                               macroSyntheticHierarchy: Double,
                               macroSyntheticMaxPop: Double,
                               macroGrowthRate: Double,
                               macroInnovationWeight: Double,
                               macroGravityDecay: Double,
                               macroInnovationDecay: Double,
                               macroEarlyAdoptersRate: Double,
                               macroUtilityStd: Double,
                               macroInitialInnovationUtility: Double
                               )



object InnovationMultiscale {

  def setup(model: InnovationMultiscale): State = {
    implicit val rng: Random = new Random(model.seed)

    import model._

    val macroModel = MacroUrbanEvolution(macroSyntheticCities, macroSyntheticHierarchy, macroSyntheticMaxPop,finalTime,
      macroGrowthRate, macroInnovationWeight, macroGravityDecay, macroInnovationDecay,macroEarlyAdoptersRate,
      macroUtilityStd,macroInitialInnovationUtility
    )
    val initialMacroState = MacroUrbanEvolution.setup(macroModel)

    val mesoModel = MesoInnovationCluster()
    val initialMesoStates = Seq.empty

    State(0, initialMacroState, initialMesoStates, rng, macroModel, mesoModel)
  }

  def multiscaleStep(model: InnovationMultiscale, state: State): State

  def run(model: InnovationMultiscale): Result = {
    Result()
  }

}
