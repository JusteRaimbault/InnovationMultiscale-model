package innovationmultiscale

case class MacroState(
                     time: Int,
                     populations: Matrix,
                     innovations: Seq[Matrix],
                     utilities: Seq[Double],
                     distanceMatrix: Matrix,
                     flows: Matrix
                     )


object MacroState {

}
