import java.io._

import scala.util.Random

object SimulatedAnnealing {
  val MAX_EVALUATIONS = 10000

  type Solution = Vector[Vector[Int]]
  case class Input()

  def evaluate(solution: Solution): Float = {
    ???
  }

  def mutate(solution: Solution): Solution = {
    ???
  }

  def generate_random_solution(input: Input): Solution = {
    ???
  }

  def clip(x: Double, min: Double, max: Double): Double = math.max(min, math.min(max, x))


  def algorithm(input: Input, max_eval: Int, seed: Long): Solution = {
    Random.setSeed(seed)

    var solution = generate_random_solution(input)
    var best_solution = solution
    var fitness = evaluate(best_solution)
    var best_fitness = fitness

    val T0 = 0.3 * best_fitness / (-math.log(0.3))
    val Tf = clip(1e-3, 0, T0)
    var T = T0

    var evaluations = 0
    var accepted = 1
    val max_neighbours = 10 * solution.length
    val max_accepted = solution.length
    val M = max_eval / max_neighbours

    while (evaluations < max_eval && accepted > 0 && T > Tf) {
        accepted = 0
        var current_evals = 0

        while (current_evals < max_neighbours && accepted < max_accepted) {
            current_evals += 1
            val sol_prime = mutate(solution)
            val fitness_prime = evaluate(sol_prime)
            val diff = fitness_prime - fitness
            val prob = math.exp(diff / T)

            if (diff > 0 || Random.nextFloat < prob) {
                solution = sol_prime
                fitness = fitness_prime
                accepted += 1
                if (fitness > best_fitness) {
                    best_fitness = fitness
                    best_solution = solution
                }
            }
        }

        evaluations += current_evals
        val beta = (T0 - Tf) / (M * T0 * Tf)
        T = T / (1 + beta * T)
    }

    best_solution
  }

  def readFile(filename: String): Input = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = bufferedSource.getLines()
    val first = lines.next().split(' ')

    val data = (for {
      (line, i) <- bufferedSource.getLines().zipWithIndex
      row = line.split(' ').toVector.map(_.toInt)
      item = ???
    } yield item).toVector

    bufferedSource.close
    Input(data, first(2).toInt, first(3).toInt, first(4).toInt, first(5).toInt)
  }

  def makeOutput(data: Solution): String = (for {
    c <- ???
  } yield c).mkString("\n")


  def main(args: Array[String]): Unit = {
    val input = readFile(args lift(0) getOrElse("input.in"))
    val file = new File(args lift(1) getOrElse("output.in"))
    val result = algorithm(input, MAX_EVALUATIONS, 0)
    val bw = new BufferedWriter(new FileWriter(file))
    val output = makeOutput(result)

    bw.write(output)
    println("Done !!!!")
    bw.close()
  }
}
