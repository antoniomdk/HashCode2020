import java.io._

import scala.annotation.tailrec

// TODO: Define Input class
// TODO: Define Solution type
// TODO: Define evaluation function


object HashCode {
  case class Car(pos: (Int, Int), step: Int, rides: Vector[Ride], canRide: Boolean)
  case class Ride(index: Int, start: (Int, Int), end: (Int, Int), es: Int, dl: Int, valid: Boolean)
  case class Input(rides: Vector[Ride], vehicles: Int, rideCount: Int, bonus: Int, steps: Int)

  type Solution = Vector[Car]

  def readFile(filename: String): Input = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = bufferedSource.getLines()
    val first = lines.next().split(' ')

    val rides = (for {
      (line, i) <- bufferedSource.getLines().zipWithIndex
      row = line.split(' ').toVector.map(_.toInt)
      ride = Ride(i, row(0) -> row(1), row(2) -> row(3), row(4), row(5), true)
    } yield ride).toVector

    bufferedSource.close
    Input(rides, first(2).toInt, first(3).toInt, first(4).toInt, first(5).toInt)
  }

  def evaluate(input: Input, solution: Solution): Float = ???

  def generateInitialSolution(input: Input): Solution = ???


  @tailrec
  def algorithm(rides: Vector[Ride], vehicles: Vector[Car], car: Int): Solution = {
    val current = vehicles(car)
    val isValid = (car: Car, r: Ride) => r.dl > (car.step + distance(car.pos, r.start) + distance(r.start, r.end))
    val validRides = rides.filter(r => r.valid)
    lazy val reallyValid = validRides.filter(isValid(current, _))

    if (validRides.isEmpty || vehicles.filter(_.canRide == false).size == vehicles.size)
      vehicles
    else if(reallyValid.isEmpty || !current.canRide)
      algorithm(rides, vehicles.updated(car, current.copy(canRide = false)), (car + 1) % vehicles.size)
    else {
      val ride = reallyValid.sortBy(evaluate(current, _)).last
      val wait = clamp0(ride.es - current.step + distance(current.pos, ride.start))
      val nStep = current.step + distance(current.pos, ride.start) + wait
      val nCar = current.copy(pos = ride.end, step = nStep, rides = current.rides :+ ride)
      val nRides = rides.updated(ride.index, ride.copy(valid = false))
      algorithm(nRides, vehicles.updated(car, nCar), (car + 1) % vehicles.size)
    }
  }

  def makeOutput(data: Solution): String = (for {
    c <- data
    sr = c.rides.map { _.index }.mkString(" ")
    r = s"${c.rides.size} $sr"
  } yield r).mkString("\n")


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
