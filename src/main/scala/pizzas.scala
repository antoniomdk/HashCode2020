import java.io._

import scala.util.Random

object SimulatedAnnealing {
  val MAX_EVALUATIONS = 10000000

  case class Pizza(index: Int, slices: Int);
  case class Input(maxSlices: Int, pizzas: Vector[Pizza])

  type Solution = List[Int]

  def evaluate(input: Input, solution: Solution): Float = solution.map(input.pizzas(_).slices).sum

  def mutate(input: Input, solution: Solution): Solution = {
    val availablePizzas = 0.to(input.pizzas.length).diff(solution)
    val newPizza = availablePizzas(Random.nextInt(availablePizzas.length - 1))
    var newSolution = solution
    var found = false

    while (!found) {
      if (evaluate(input, newSolution) + input.pizzas(newPizza).slices < input.maxSlices) {
        newSolution = newPizza :: newSolution
        found = true
      } else {
        val i = Random.nextInt(input.pizzas.length)
        newSolution = newSolution.zipWithIndex.filter(x => x._2 != i).map(_._1)
      }
    }

    newSolution
  }

  def generate_random_solution(input: Input): Solution = List()

  def clip(x: Double, min: Double, max: Double): Double = math.max(min, math.min(max, x))


  def algorithm(input: Input, max_eval: Int, seed: Long): Solution = {
    Random.setSeed(seed)

    var solution = generate_random_solution(input)
    var best_solution = solution
    var fitness = evaluate(input, best_solution)
    var best_fitness = fitness

    val T0 = 1000.0
    val Tf = 0.0001
    var T = T0

    var evaluations = 0
    var accepted = 1
    val max_neighbours = input.pizzas.length
    val max_accepted = input.pizzas.length
    val M = max_eval / max_neighbours

    while (evaluations < max_eval && accepted > 0 && T > Tf) {
      accepted = 0
      var current_evals = 0

      while (current_evals < max_neighbours && accepted < max_accepted) {
        current_evals += 1
        val sol_prime = mutate(input, solution)
        val fitness_prime = evaluate(input, sol_prime)
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
      println(T)
    }

    best_solution
  }

  def readFile(path: String): Input = {
    val lines = scala.io.Source.fromFile(new File(path)).getLines().toList
    val maxSlices = lines(0).split(" ")(0).toInt
    val pizzas = lines(1).split(" ").zipWithIndex.map({
      value => Pizza(value._2, value._1.toInt)
    }).toVector
    Input(maxSlices, pizzas)
  }

  def makeOutput(solution: Solution): String = {
    val selected = solution.sorted
    selected.length.toString + "\n" + selected.mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val dataFolder = new File("data")

    dataFolder.listFiles().foreach(file => {
      val path = file.getPath.replace(".in", ".out")
      val input = readFile(file.getPath)

      val result = algorithm(input, MAX_EVALUATIONS, 0)
      val bw = new BufferedWriter(new FileWriter(path))
      val output = makeOutput(result)

      bw.write(output)
      println("Done !!!!")
      bw.close()

      println("next")
    })
  }
}
