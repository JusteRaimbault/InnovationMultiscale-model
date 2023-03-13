package innovationmultiscale

import scala.util.Random

object RunModel extends App {

  val rng: Random = new Random()

  val model = InnovationMultiscale(
    seed = rng.nextLong(),
    finalTime = 50,
    setupSyntheticCities = 10,
    setupSyntheticHierarchy = 1.0,
    setupSyntheticMaxPop = 10000.0,
    setupFirmSizeScaling = 1.0,
    setupLargestFirmSize = 100.0,
    setupFirmsNumberScaling = 1.0,
    setupLargestClusterSize = 20.0,
    setupLargestFirmSizeScaling = 1.0,
    macroGrowthRate = 0.0,
    macroInnovationWeight = 0.01,
    macroGravityDecay = 0.5,
    macroInnovationDecay = 0.3,
    macroEarlyAdoptersRate = 0.2,
    macroUtilityStd = 1.0,
    macroInitialInnovationUtility = 1.0,
    mesoGenomeSize = 10,
    mesoTimeSteps = 50,
    mesoCrossOverProba = 0.2,
    mesoCrossOverShare = 0.5,
    mesoMutationProba = 0.01,
    mesoMutationAmplitude = 1.0,
    mesoCurrentProductShare = 0.5,
    mesoInteractionProba = 0.00001,
    mesoDistanceDecay = 100.0,
    mesoToMacroInnovationThreshold = 1.0,
    macroToMesoCrossoverMaxUpdate = 0.01,
    macroToMesoMutationMaxUpdate = 0.001,
    macroToMesoExchangeMaxUpdate =  -0.000001
  )

  val result = model.run

  //println(s"Macro utilities = ${result.macroUtilities.toSeq}")
  println(s"Macro utility = ${result.averageMacroUtility}")
  //println(s"Macro diversities = ${result.macroDiversities.toSeq}")
  println(s"Macro diversity = ${result.averageMacroDiversity}")
  println(s"Macro innovation = ${result.averageMacroInnovation}")
  println(s"Meso diversity = ${result.averageMesoProductDiversity}")
  println(s"Meso fitness = ${result.averageMesoBestFitness}")

  //println(result.macroUtilities.length)
  //println(result.mesoTimeSeries.length)
  //println(result.mesoTimeSeries.map(_.length).toSeq)
  println(s"Psi (utility) = ${Statistics.psiWithStd(result.mesoTimeSeries,result.macroUtilities)(rng)}")
  println(s"Delta (utility) = ${Statistics.deltaWithStd(result.mesoTimeSeries,result.macroUtilities)(rng)}")
  println(s"Gamma (utility) = ${Statistics.gammaWithStd(result.mesoTimeSeries,result.macroUtilities)(rng)}")

  println(s"Psi (diversity) = ${Statistics.psiWithStd(result.mesoTimeSeries,result.macroDiversities)(rng)}")
  println(s"Delta (diversity) = ${Statistics.deltaWithStd(result.mesoTimeSeries,result.macroDiversities)(rng)}")
  println(s"Gamma (diversity) = ${Statistics.gammaWithStd(result.mesoTimeSeries,result.macroDiversities)(rng)}")

}
