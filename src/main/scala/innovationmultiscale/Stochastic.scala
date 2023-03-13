package innovationmultiscale

import scala.util.Random

object Stochastic {

  sealed trait Distribution {
    def rng: Random
    def draw: Double
  }

  case class NormalDistribution(mu: Double, sigma: Double)(implicit localRng: Random) extends Distribution {
    override def rng: Random = localRng
    override def draw: Double = mu + sigma*rng.nextGaussian()
  }

  case class LogNormalDistribution(mu: Double, sigma: Double)(implicit localRng: Random) extends Distribution {
    override def rng: Random = localRng
    override def draw: Double = math.exp(mu + sigma*rng.nextGaussian())
  }
  object LogNormalDistribution {
    def fromMoments(average: Double, std: Double)(implicit localRng: Random): LogNormalDistribution = {
      assert(average>0,"Average of a log-normal should be strictly positive")
      assert(std>0,"Std of a log-normal should be strictly positive")
      assert(math.log(std)<math.log(average)-0.5,"Std of log normal too low regarding average")
      LogNormalDistribution(
        mu = 2 * math.log(average) - math.log(std) - 0.5,
        sigma = math.sqrt(1 - 2*(math.log(average) - math.log(std)))
      )
    }
  }


}
