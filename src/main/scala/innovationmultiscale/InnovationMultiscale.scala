package innovationmultiscale

case class InnovationMultiscale(
                               seed: Long
                               )



object InnovationMultiscale {

  def setup(model: InnovationMultiscale): State = {
    val initialMacroState = MacroUrbanEvolution.setup()
    State(0)
  }

}
