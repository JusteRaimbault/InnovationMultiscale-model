package innovationmultiscale

case class State(
                time: Int,
                macroState: MacroState,
                mesoStates: Seq[MesoState]
                )

object State {

}