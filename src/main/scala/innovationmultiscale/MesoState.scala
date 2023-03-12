package innovationmultiscale

case class MesoState(firms: Seq[MesoInnovationCluster.Firm],
                     interactionIntensity: Double,
                     time: Int)
