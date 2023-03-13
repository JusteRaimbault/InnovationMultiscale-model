package innovationmultiscale

import org.apache.commons.math3.random.RandomDataGenerator

import scala.jdk.CollectionConverters.IterableHasAsJava
import scala.util.Random

object ScaleCoupling {

  /**
   * size of firms: rank size law globally
   * @param initialMacroState
   * @return
   */
  def initialFirmSizes(initialMacroState: MacroState, firmSizeScaling : Double, largestFirmSize: Double,
                       firmsNumberScaling: Double, largestClusterSize: Double, largestFirmSizeScaling: Double
                      )(implicit rng: Random): Seq[Seq[Int]] = {
    // number of firms for each area
    val p0 = initialMacroState.populations.getCol(0).max
    val firmNumbers: Seq[Int] = initialMacroState.populations.getCol(0).flatValues.map(
      p => math.round(largestClusterSize*math.pow(p / p0, firmsNumberScaling)).toInt)

    // global firm sizes
    val firmSizes = (1 to firmNumbers.sum).map(
      k => math.round(largestFirmSize*math.pow(k, -firmSizeScaling)).toInt)

    // largest firm sizes
    val largestFirmSizes: Seq[Int] = initialMacroState.populations.getCol(0).flatValues.map(
      p => math.round(largestFirmSize*math.pow(p / p0, largestFirmSizeScaling)).toInt)

    val sampler = new RandomDataGenerator()
    sampler.reSeed(rng.nextLong())
    // sample global sizes - with redraw
    firmNumbers.zip(largestFirmSizes).map { case (n,s) =>
      sampler.nextSample(firmSizes.filter(_ < s).asJavaCollection, n).toSeq.map(_.asInstanceOf[Int])
    }
  }

  /**
   * meso -> macro
   */
  def innovativeCities(cycleStates: Seq[Seq[MesoState]], mesoToMacroInnovationThreshold: Double): Seq[Int] = {
    val citiesRelativePerf = cycleStates.map{cityCycle =>
      val bestFitness = cityCycle.last.firms.map(_.currentFitness).max
      val avgFitness = cityCycle.last.firms.map(_.currentFitness).sum / cityCycle.last.firms.size.toDouble
      (bestFitness - avgFitness)/avgFitness
    }
    //println(s"Relative perfs = $citiesRelativePerf")
    citiesRelativePerf.zipWithIndex.filter(_._1>mesoToMacroInnovationThreshold).map(_._2)
  }

  /**
   * macro -> meso
   *   rel delta pop: deltaP / max(delta P)
   *
   *   with genome/utilities? ~ hypothesis more difficult to interpret
   */
  def macroToMesoUpdateMeso(
                             mesoAfterCycle: Seq[Seq[MesoState]],
                             mesoModels: Seq[MesoInnovationCluster],
                             //previousMacroState: MacroState,// not needed - all pop in Matrix
                             macroState: MacroState,
                             macroToMesoCrossoverMaxUpdate: Double,
                             macroToMesoMutationMaxUpdate: Double,
                             macroToMesoExchangeMaxUpdate: Double
                           ): Seq[(MesoInnovationCluster, Seq[MesoState])] = {
    val deltaPops = (macroState.populations.getCol(macroState.time)- macroState.populations.getCol(macroState.time-1)).flatValues
    val deltaPopMax = deltaPops.max
    val relDeltaPop = deltaPops.map(_ / deltaPopMax)
    val updatedMesoModels = mesoModels.zip(relDeltaPop).map{case (m,delta) =>
      m.copy(crossOverProba = m.crossOverProba*(1+macroToMesoCrossoverMaxUpdate*delta),
        mutationProba=m.mutationProba*(1+macroToMesoMutationMaxUpdate*delta),
        interactionProba=m.interactionProba*(1+macroToMesoExchangeMaxUpdate*delta)
      )
    }
    mesoModels.zip(mesoAfterCycle)
  }

}
