package utils

trait Primes {
  def isPrime(num: Long): Boolean
  def getNthPrime(num: Int): Long
  def getPrimes: Seq[Long]
  def getPrimeFactors(num: Long): List[Long]
  def commonMultiply(num1: Long, num2: Long): Long
  def commonDivisor(num1: Long, num2: Long): Long
  def getPrimeFactorsTuple(num: Long): List[(Long, Int)]
  def primeFactorsTuplePower(primes: List[(Long, Int)], power: Int): List[(Long, Int)]
  def getPrimeFactorsTuplePower(num: Long, power: Int): List[(Long, Int)]
  def getNumberFromFactorsTuple(primes: List[(Long, Int)]): Long
  def simpleStream: Seq[Int]
}

class ErastotenStreamPrimes extends Primes {
  private def seaveHelp(numbers: Stream[Long], prime: Long) = numbers.filter(_ % prime != 0)
  private def seaveOld(numbers: Stream[Long]): Stream[Long] = numbers.head #:: seave(numbers.tail.filter(_ % numbers.head != 0))
  private def seave(numbers: Stream[Long]): Stream[Long] = numbers.head #:: seave(seaveHelp(numbers.tail, numbers.head))
  def streamL(i: Long = 1): Stream[Long] = i #:: streamL(i + 1)
  //lazy val primes = seave(Stream.from(2L))
  private lazy val primes = seave(streamL(2L))
  def isPrime(num: Long): Boolean = getPrimes.takeWhile { _ <= num }.contains(num)
  def getNthPrime(num: Int): Long = getPrimes.drop(num).head
  def getPrimes: Seq[Long] = primes
  def getPrimeFactors(num: Long) = {
    def _inner(rest: Long, primesTail: Seq[Long]): List[Long] = if (rest < 2) List() else if (isPrime(rest)) List(rest) else if (rest % primesTail.head == 0) primesTail.head :: _inner(rest / primesTail.head, primesTail) else _inner(rest, primesTail.tail)
    _inner(num, getPrimes)
  }
  def getPrimeFactorsTuple(num: Long): List[(Long, Int)] = {
    val p = getPrimeFactors(num)
    p.groupBy(identity).toList.map((p) => (p._1, p._2.length))
  }

  def primeFactorsTuplePower(primeFactors: List[(Long, Int)], power: Int): List[(Long, Int)] = primeFactors.map((p) => (p._1, p._2 * power))
  def getPrimeFactorsTuplePower(num: Long, power: Int): List[(Long, Int)] = primeFactorsTuplePower(getPrimeFactorsTuple(num), power)

  def getNumberFromFactorsTuple(factors: List[(Long, Int)]): Long = factors.map((p: (Long, Int)) => Math.pow(p._1, p._2).toLong).fold(1L)(_ * _)

  def commonMultiply(num1: Long, num2: Long): Long = {
    def mergeFactors(fac1: List[Long], fac2: List[Long]): List[Long] = if (fac1.isEmpty) fac2 else if (fac2.isEmpty) fac1 else if (fac1.head < fac2.head) fac1.head :: mergeFactors(fac1.tail, fac2) else if (fac2.head < fac1.head) fac2.head :: mergeFactors(fac1, fac2.tail) else fac2.head :: mergeFactors(fac1.tail, fac2.tail)
    val primeFactors1 = getPrimeFactors(num1)
    val primeFactors2 = getPrimeFactors(num2)
    mergeFactors(primeFactors1, primeFactors2).fold(1L)(_ * _)
  }

  def commonDivisorPrimes(num1: Long, num2: Long): Long = {
    def mergeFactors(fac1: List[Long], fac2: List[Long], accu: Long): Long = if (fac1.isEmpty || fac2.isEmpty) accu else if (fac1.head < fac2.head) mergeFactors(fac1.tail, fac2, accu) else if (fac2.head < fac1.head) mergeFactors(fac1, fac2.tail, accu) else mergeFactors(fac1.tail, fac2.tail, accu * fac2.head)
    val primeFactors1 = getPrimeFactors(num1)
    val primeFactors2 = getPrimeFactors(num2)
    mergeFactors(primeFactors1, primeFactors2, 1)
  }

  def commonDivisor(num1: Long, num2: Long): Long = {
    def _inner(num1: Long, num2: Long) = if (num1 % num2 == 0) num2 else commonDivisor(num2, num1 % num2)
    _inner(Math.max(num1, num2), Math.min(num1, num2))
  }

  private def recurve0(numbers: Stream[Int]) = numbers.filter(_ % 3 != 0)
  private def recurve(numbers: Stream[Int]): Stream[Int] = numbers.head #:: recurve0(numbers.tail)
  def simpleStream: Seq[Int] = recurve(Stream.from(2))
}

class SimplePrimes extends ErastotenStreamPrimes {
  override def getPrimes: Seq[Long] = super.getPrimes

  override def isPrime(n: Long): Boolean = if (n == 1) false else if (n < 4) true //2 and 3 are prime
  else if (n % 2 == 0) false else if (n < 9) true //we have already excluded 4,6 and 8.
  else if (n % 3 == 0) false else {
    val max = (Math.floor(Math.sqrt(n))).longValue()
    def _inner(max: Long, f: Long = 5):Boolean =  if(f>max) true else if(n % f ==0) false else if (n % (f+2)==0) false else _inner(max, f+6)  
    _inner(max)
  }
}

object Primes {
  //def apply() = new ErastotenStreamPrimes()
  def apply() = new SimplePrimes()
}