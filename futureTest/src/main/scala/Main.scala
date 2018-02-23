import scala.concurrent.duration.Duration
import scala.concurrent.{Await,Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Main{

  //isPrime(n) == true  <=>  n is prime
  def isPrime(n: Int): Boolean = (2 until n-1).forall(n % _ != 0)

  //logging function
  def log(message: String): Unit = {
    val thread = Thread.currentThread.getName()
    println(s"[${thread}] ${message}")
  }

  //Square function
  def sqr(x: Double) = x * x

  //benchmarking function
  def measureTime[B](warmupIterations: Int,
                     measuredIterations: Int,
                     block: => B,
                     label: String): Unit = {
    Range(1,warmupIterations).foreach{
      i =>
        val t0 = System.nanoTime()
        block
        val t1 = System.nanoTime()
    }

    val times = scala.collection.mutable.Queue.empty[Double]
    Range(1,measuredIterations).foreach{
      i =>
        val t0 = System.nanoTime()
        block
        val t1 = System.nanoTime()
        times += (t1 - t0)/1000000.0
        System.gc()
    }

    val mean = math.floor(times.sum / measuredIterations.toFloat * 100)/100
    val variance = times.map(t => sqr(t - mean)).sum / measuredIterations

    println("[BENCHMARK] Measured times : " + times.map(t => math.floor(t * 100)/100))
    println("[BENCHMARK] " + label + "," + mean + "," + math.floor(math.sqrt(variance) * 1000)/1000)
  }

  //Check if every integer of a given list is prime (sequential vesion)
  def arePrime(list: List[Int], withLogs: Boolean): Boolean = {
    if(list.tail.isEmpty){
      val b = isPrime(list.head)
      if(withLogs) log(s"isPrime(${list.head}) -> $b")
      b
    }
    else{
      val (l1,l2) = list.splitAt(list.size / 2)
      val b1 = arePrime(l1,withLogs)
      val b2 = arePrime(l2,withLogs)
      if(withLogs) log(s"isPrime(${list}) -> ${b1 && b2}")
      b1 && b2
    }
  }

  //Check if every integer of a given list is prime (concurrent version)
  def concurrentArePrime(list: List[Int], withLogs: Boolean): Future[Boolean] = Future.unit.flatMap{ _ =>
    if(list.tail.isEmpty){
      val b = isPrime(list.head)
      if(withLogs) log(s"isPrime(${list.head}) -> $b")
      Future{b}
    }
    else{
      val (l1,l2) = list.splitAt(list.size / 2)
      val b1 = concurrentArePrime(l1,withLogs)
      val b2 = concurrentArePrime(l2,withLogs)
      val combinedFuture = b1.zipWith(b2)((v1,v2) => v1 && v2)
      if(withLogs) log(s"isPrime(${list}) -> ${combinedFuture}")
      combinedFuture
    }
  }



  def main(args: Array[String]): Unit = {
    val integerList = List(179426549,3,5,13,43,179426263,15487469,3,5,13,179426263,15487469,3,5,13,
      179426263,15487469,3,5,13,179426263,15487469,3,5,13,43,179426263,15487469,3,5,13,43,179426263,
      15487469,3,5,13,43,179426263,15487469)
    
    //Sequential version testing
    //log(s"FINAL RESULT : ${arePrime(integerList,withLogs=true)}")

    //Sequential version benchmarking
    measureTime(10,10,arePrime(integerList,withLogs=false),"sequential")

    //Concurrent version testing
    //log(s"FINAL RESULT : ${Await.result(concurrentArePrime(integerList,withLogs=true), Duration.Inf)}")

    //Concurrent version benchmarking
    measureTime(10,10,Await.result(concurrentArePrime(integerList,withLogs=false), Duration.Inf),"concurrent")
  }
}