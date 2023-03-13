package innovationmultiscale

import scala.util.Random

case class State(
                time: Int,
                macroState: MacroState,
                mesoStates: Seq[Seq[MesoState]],
                rng: Random,
                macroModel: MacroUrbanEvolution,
                mesoModels: Seq[MesoInnovationCluster]
                )

object State {

}