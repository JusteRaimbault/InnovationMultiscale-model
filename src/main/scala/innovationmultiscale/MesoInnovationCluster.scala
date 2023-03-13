package innovationmultiscale

import scala.jdk.CollectionConverters.IterableHasAsJava
import scala.util.Random
import org.apache.commons.math3.random.RandomDataGenerator

case class MesoInnovationCluster(
                                  genomeSize: Int,
                                  timeSteps: Int,
                                  crossOverProba: Double,
                                  crossOverShare: Double,
                                  mutationProba: Double,
                                  mutationAmplitude: Double,
                                  currentProductShare: Double,
                                  interactionProba: Double,
                                  distanceDecay: Double,
                                  fitness: MesoInnovationCluster.Fitness
                                ) {

}


object MesoInnovationCluster {

  sealed trait Fitness {
    def genome(rng: Random): Seq[Double]
    def fitness(g: Seq[Double]): Double
  }

  case class GeneralizedRastriginFitness(m: Array[Array[Double]], genomeSize: Int) extends Fitness {
    override def genome(rng: Random): Seq[Double] = Seq.fill(genomeSize)(10.0*(rng.nextDouble()-0.5)) // in [-5,5]
    override def fitness(g: Seq[Double]): Double = {
      val res = m.map{row =>
        row.zip(g).map{case (mij, gj)=>
          mij*(gj*gj - 10.0*math.cos(2.0*math.Pi*gj))
        }.sum
      }.sum
      -1.0*res
    }
  }

  def randomGeneralizedRastrigin(size: Int)(implicit rng: Random): GeneralizedRastriginFitness = GeneralizedRastriginFitness(
    Array.fill(size,size)(rng.nextDouble()),
    size
  )


  case class Employee(
                       id: Int,
                       currentIdeas: Seq[Double]
                     ){}

  object Employee {
    def apply(i: Int, genomeFunction: Int => Seq[Double]): Employee = Employee(i,genomeFunction(i))
  }


  case class Firm(
                   location: (Double, Double),
                   employees: Seq[Employee],
                   currentProduct: Seq[Double],
                   currentFitness: Double
                 ){
  }


  object Firm {

    /**
     * Firm with random location, employees and product
     *  return firm and remaining employees
     * @param size firm size
     * @param allEmployees all employee ids
     * @return
     */
    def randomFirm(size: Int, allEmployees: Seq[Employee], fitness: Fitness)(implicit rng: Random): (Firm, Seq[Employee]) = {
      val loc = (100.0*rng.nextDouble(), 100.0*rng.nextDouble())
      // sample without replacement
      val sampler = new RandomDataGenerator()
      sampler.reSeed(rng.nextLong())
      val employees: Seq[Employee] = sampler.nextSample(allEmployees.asJavaCollection, size).map(_.asInstanceOf[Employee]).toSeq
      // random product at the beginning: take the first
      val product = employees.head.currentIdeas
      // remaining employees - sampling not optimal
      val remaining = allEmployees.filter(!employees.contains(_))
      (Firm(loc, employees, product, fitness.fitness(product)), remaining)
    }

    def innovate(firm: Firm,
                 crossOverProba: Double,
                 crossOverShare: Double,
                 mutationProba: Double,
                 mutationAmplitude: Double,
                 currentProductShare: Double,
                 fitness: Seq[Double] => Double
                )(implicit rng: Random): Firm = {
      // each employee has crossOverProba chances of exchanging ideas with an other at random
      // to simplify, copy a random sequence of the other genome
      val exchangedIdeas = firm.employees.map{e =>
        if (rng.nextDouble()<crossOverProba){
          val copied = firm.employees(rng.nextInt(firm.employees.length)).currentIdeas
          e.copy(currentIdeas = e.currentIdeas.zip(copied).map{case (s,o) => if (rng.nextDouble()<crossOverShare) o else s})
        }
        else e
      }


      // mutate ideas (uniform with fixed amplitude)
      val mutatedIdeas = exchangedIdeas.map{e =>
        e.copy(currentIdeas = e.currentIdeas.map{i => if (rng.nextDouble()<mutationProba) i - mutationAmplitude/2.0 + mutationAmplitude/2.0*rng.nextDouble() else i})
      }

      // find best idea
      val fitnessesAndIndivs = mutatedIdeas.map(e => (fitness(e.currentIdeas), e))
      val best = fitnessesAndIndivs.maxBy(_._1)
      val finalIndivs = fitnessesAndIndivs.map{case (_,e) =>
        if (e==best._2) e
        else {
          if (rng.nextDouble()< currentProductShare) e.copy(currentIdeas = best._2.currentIdeas)
          else e
        }
      }

      firm.copy(employees = finalIndivs, currentProduct = best._2.currentIdeas, currentFitness = best._1)
    }

    def exchangeInformal(firms: Seq[Firm],
                         interactionProba: Double,
                         distanceDecay: Double,
                         crossOverShare: Double
                        )(implicit rng: Random): (Seq[Firm], Double) = {
      var totalInteractions = 0
      var potentialInteractions = 0
      (firms.map{fi =>
        val updatedEmployees = fi.employees.map{ei =>
          val newIdeas = ei.currentIdeas.toArray // mutable locally
          firms.foreach { fj =>
            val expdij = math.exp(-1.0*math.sqrt(math.pow(fi.location._1-fj.location._1, 2.0)+math.pow(fi.location._2-fj.location._2, 2.0))/distanceDecay)
            if (fj != fi) {
              potentialInteractions = potentialInteractions + fi.employees.size*fj.employees.size
              fj.employees.foreach { ej =>
                if (rng.nextDouble() < interactionProba * expdij) {
                  ej.currentIdeas.zipWithIndex.foreach { case (i, ind) => if (rng.nextDouble() < crossOverShare) newIdeas(ind) = i }
                  totalInteractions = totalInteractions + 1
                }
              }
            }
          }
          ei.copy(currentIdeas = newIdeas.toSeq)
        }
        fi.copy(employees = updatedEmployees)
      },
        totalInteractions.toDouble/potentialInteractions.toDouble)
    }





  }


  /**
   * setup one meso state given sizes
   * @param sizes
   * @param fitness
   * @param rng
   * @return
   */
  def setup(sizes: Seq[Int], fitness: Fitness)(implicit rng: Random): MesoState = {
    val firmsNumber = sizes.size
    val employees: Seq[Employee] = (1 to sizes.sum).map{i =>Employee(i, fitness.genome(rng))}
    val res = Iterator.iterate((Seq.empty[Firm], employees, 0)){
      case (firms, employees, sizeIndex) =>
        val (newfirm, remainingEmployees) =  Firm.randomFirm(sizes(sizeIndex), employees, fitness)
        (firms++Seq(newfirm), remainingEmployees, sizeIndex+1)
    }.takeWhile(_._1.size<firmsNumber).toSeq.last
    val (lastFirm, _) = Firm.randomFirm(sizes.last, res._2, fitness)
    val firms = res._1++Seq(lastFirm)
    MesoState(firms, 0.0, 0)
  }


  def setupCycle(states: Seq[MesoState], model: MesoInnovationCluster)(implicit rng: Random): (MesoState, MesoInnovationCluster) = {
    val newmodel = model.copy(fitness = MesoInnovationCluster.randomGeneralizedRastrigin(model.genomeSize))
    val sizes = states.last.firms.map(_.employees.size)
    //println(sizes)
    val newstate = setup(sizes, newmodel.fitness)
    (newstate, newmodel)
  }


  def mesoStep(state: MesoState, model: MesoInnovationCluster)(implicit rng: Random): MesoState = {
    import model._

    // innovate within firms
    val innovFirms = state.firms.map { f =>
      Firm.innovate(f, crossOverProba, crossOverShare, mutationProba, mutationAmplitude, currentProductShare, fitness.fitness)
    }

    val (exchangedFirms, interactionIntensity) = Firm.exchangeInformal(innovFirms, interactionProba, distanceDecay, crossOverShare)

    state.copy(firms = exchangedFirms, interactionIntensity = interactionIntensity, time = state.time + 1)
  }


  /**
   * one cycle for one innovation cluster
   * @param model
   * @param previousCycleStates
   * @param rng
   * @return
   */
  def mesoCycle(model: MesoInnovationCluster, previousCycleStates: Seq[MesoState])(implicit rng: Random): (Seq[MesoState], MesoInnovationCluster) = {
    val (initialState, newmodel) = setupCycle(previousCycleStates, model)
    def f(s: MesoState): MesoState = mesoStep(s, newmodel)
    (Iterator.iterate(initialState)(f).takeWhile(_.time <= newmodel.timeSteps).toSeq, newmodel)
  }

}

