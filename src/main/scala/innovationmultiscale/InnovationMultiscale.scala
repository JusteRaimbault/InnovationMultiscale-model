package innovationmultiscale

import scala.util.Random

case class InnovationMultiscale(
                               seed: Long,
                               finalTime: Int,
                               setupSyntheticCities: Int,
                               setupSyntheticHierarchy: Double,
                               setupSyntheticMaxPop: Double,
                               setupFirmSizeScaling: Double,
                               setupLargestFirmSize: Double,
                               setupFirmsNumberScaling: Double,
                               setupLargestClusterSize: Double,
                               setupLargestFirmSizeScaling: Double,
                               macroGrowthRate: Double,
                               macroInnovationWeight: Double,
                               macroGravityDecay: Double,
                               macroInnovationDecay: Double,
                               macroEarlyAdoptersRate: Double,
                               macroUtilityStd: Double,
                               macroInitialInnovationUtility: Double,
                               mesoGenomeSize: Int,
                               mesoTimeSteps: Int,
                               mesoCrossOverProba: Double,
                               mesoCrossOverShare: Double,
                               mesoMutationProba: Double,
                               mesoMutationAmplitude: Double,
                               mesoCurrentProductShare: Double,
                               mesoInteractionProba: Double,
                               mesoDistanceDecay: Double,
                               mesoToMacroInnovationThreshold: Double,
                               macroToMesoCrossoverMaxUpdate: Double,
                               macroToMesoMutationMaxUpdate: Double,
                               macroToMesoExchangeMaxUpdate: Double
                               ) {
  def run: Result = InnovationMultiscale.run(this)
}



object InnovationMultiscale {

  def setup(model: InnovationMultiscale): State = {
    implicit val rng: Random = new Random(model.seed)

    import model._

    val macroModel = MacroUrbanEvolution(setupSyntheticCities, setupSyntheticHierarchy, setupSyntheticMaxPop,finalTime,
      macroGrowthRate, macroInnovationWeight, macroGravityDecay, macroInnovationDecay,macroEarlyAdoptersRate,
      macroUtilityStd,macroInitialInnovationUtility
    )
    val initialMacroState = MacroUrbanEvolution.setup(macroModel)

    val mesoSizes = ScaleCoupling.initialFirmSizes(initialMacroState, setupFirmSizeScaling, setupLargestFirmSize,setupFirmsNumberScaling, setupLargestClusterSize, setupLargestFirmSizeScaling)

    val mesoModels = mesoSizes.map(_ =>
      MesoInnovationCluster(mesoGenomeSize, mesoTimeSteps, mesoCrossOverProba, mesoCrossOverShare,
        mesoMutationProba, mesoMutationAmplitude, mesoCurrentProductShare, mesoInteractionProba, mesoDistanceDecay,
        MesoInnovationCluster.randomGeneralizedRastrigin(mesoGenomeSize))
    )
    //println(mesoSizes)
    val initialMesoStates = mesoSizes.zip(mesoModels).map{case (s, m) => Seq(MesoInnovationCluster.setup(s, m.fitness))}

    State(0, initialMacroState, initialMesoStates, rng, macroModel, mesoModels)
  }

  def multiscaleStep(model: InnovationMultiscale, state: State): State = {
    implicit val rng: Random = state.rng

    //println(state.mesoStates.map(_.last.firms.map(_.employees.size)))// at setup one firm is lost: ?
    // meso
    val mesoAfterCycle: (Seq[Seq[MesoState]], Seq[MesoInnovationCluster]) = state.mesoStates.zip(state.mesoModels).map{
      case (previousCycleStates,m) =>
        MesoInnovationCluster.mesoCycle(m,previousCycleStates)
    }.unzip

    // meso -> macro
    val mesoToMacroInnovativeCities: Seq[Int] = ScaleCoupling.innovativeCities(mesoAfterCycle._1, model.mesoToMacroInnovationThreshold)
    //println(s"Innovative cities: $mesoToMacroInnovativeCities")

    // macro
    val newMacroState = MacroUrbanEvolution.macroStep(state.macroModel, state.macroState, mesoToMacroInnovativeCities)

    // macro -> meso
    val macroToMesoUpdateMeso: Seq[(MesoInnovationCluster, Seq[MesoState])] =
      ScaleCoupling.macroToMesoUpdateMeso(mesoAfterCycle._1, mesoAfterCycle._2, newMacroState,
        model.macroToMesoCrossoverMaxUpdate, model.macroToMesoMutationMaxUpdate, model.macroToMesoExchangeMaxUpdate)

    state.copy(
      time = state.time + 1,
      macroState = newMacroState,
      mesoStates = macroToMesoUpdateMeso.map(_._2),
      mesoModels = macroToMesoUpdateMeso.map(_._1)
    )
  }

  def run(model: InnovationMultiscale): Result = {
    val initialState: State = setup(model)
    def f(s: State): State = multiscaleStep(model, s)
    val states = Iterator.iterate(initialState)(f).takeWhile(_.time <= model.finalTime - 1).toSeq
    Result(states)
  }

}
