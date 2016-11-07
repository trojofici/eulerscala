package problems

import utils.Primes

import util._
import scala.Stream
import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper

object Problems27_ {
  def problem27 = {
    val p = Primes()
    val koefs = for {
      a <- -1000 + 1 until 1000 - 1
      b <- -1000 until 1000
      cd = p.commonDivisor(Math.abs(a), Math.abs(b))
      if (cd == 1 || cd > 60)
      val primeCount = Stream.from(0).takeWhile((n: Int) => p.isPrime(n * n + a * n + b)).length
      //if(primeCount>60)
    } yield (a * b, a, b, primeCount)
    koefs.maxBy(_._4)
  }

  def problem29 = {
    val p = Primes()
    val max = 100 + 1
    val koefs = for {
      a <- 2 until max
      b <- 2 until max
      //a <- 6 until 7
      //b <- 6 until 7
      primes = p.getPrimeFactorsTuplePower(a, b)
    } yield primes
    koefs.distinct.length
  }

  def problem30 = {
    val na = 7;
    val maxAdd = Math.pow(9, na)
    def addDigit(digits: (List[Int], Int, Int)): Stream[(List[Int], Int, Int)] = for {
      i <- (0 until 10).toStream
      newSum = digits._2 + Math.pow(i, na).toInt
      newNumber = digits._3 * 10 + i
      //don't use new values if not viable
      if (newSum == newNumber || 10 * newNumber <= newSum + maxAdd)
    } yield (i :: digits._1, newSum, newNumber)

    def addLevel(digits: Stream[(List[Int], Int, Int)]): Stream[(List[Int], Int, Int)] = {
      val added = for {
        item <- digits
      } yield addDigit(item)
      val lolo = added.flatten
      if (lolo.isEmpty) digits else digits #::: addLevel(lolo)
    }
    addLevel(addDigit((List(), 0, 0)).drop(1)).filter((p) => (p._2 == p._3 && p._2 != 1)).toList.map(_._3).sum
    //addLevel(addDigit((List(), 0, 0)).drop(1)).filter((p) => (p._2 == p._3 && p._2 != 1)).toList.map(_._3)
  }

  def problem31b = {

    def count(n: Int, pennies: List[Int]): Int = pennies match {
      case _ if n == 0 => 1
      case 1 :: _ => 1
      case p :: tail => {
        val max = n / p
        Stream range (0, max + 1) map (i => count(n - p * i, tail)) sum
      }
    }

    val pennies = 200 :: 100 :: 50 :: 20 :: 10 :: 5 :: 2 :: 1 :: Nil
    val answer = count(200, pennies)
    answer
  }

  def problem31 = {
    //1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
    val targetSum = 200
    val allCoins = List(200, 100, 50, 20, 10, 5, 2, 1)
    def addCoin(coins: (List[Int], Int)): Stream[(List[Int], Int)] = for {
      newCoin <- allCoins.dropWhile(!coins._1.isEmpty && _ > coins._1.head).toStream
      newSum = coins._2 + newCoin
      //don't use new values if not viable
      if (newSum <= targetSum)
    } yield (newCoin :: coins._1, newSum)

    def addLevel(coins: Stream[(List[Int], Int)]): Stream[(List[Int], Int)] = {
      val added = for {
        item <- coins
      } yield addCoin(item)
      val lolo = added.flatten
      if (lolo.isEmpty) coins else coins #::: addLevel(lolo)
    }
    addLevel(Stream((List(), 0))).filter((p) => p._2 == targetSum).length

  }
  
  def problem32 = {
    val digits = List.range(1,9)
    val digitsR = 1 to 9
    val perm = digitsR.permutations
    //digitsR.
    def calculateValues(per:IndexedSeq[Int], frst:Int, sec:Int):(Int, Int, Int) = {
      //val nums = per.scanRight(0)((x:Int, y:Int) => x+10*y)
      //val one = per.take(frst).foldRight(0)((x:Int, y:Int) => x+10*y)
      val one = per.take(frst).foldLeft(0)((y:Int, x:Int) => x+10*y)
      val two = per.drop(frst).take(sec).foldLeft(0)((y:Int, x:Int) => x+10*y)
      val three =  per.drop(frst+sec).foldLeft(0)((y:Int, x:Int) => x+10*y)
     // println(one+":"+two+":"+three)
      (one, two, three)
    }
    val nums  = for {
      p <- perm
      l1 <- 1 to 4
      l2 <- 5 - l1 to 9 - l1
      //basic range
      //l1 <- 1 to 7
      //l2 <- 1 to 8 - l1 
      vals = calculateValues(p, l1, l2)
      if(vals._1*vals._2==vals._3)
    } yield vals._3
    nums.toList.distinct.sum
  }
  
  def problem33 = {
    val a = for {
      x <- 1 to 9
      y <- 1 to 9
      z <- 1 to 9
      if(9*x*z+y*z == 10*x*y && !(x==y && y==z))
    } yield (10*x+y, 10*y+z)
    val (a1, a2) = a.unzip
    val nom = a1.product
    val denom = a2.product
    val div = Primes().commonDivisor(nom, denom)
    denom/div
    
  }
  
  def problem34 = {
    val maxAdd = (1 to 9).product
    def addDigit(digits: (List[Int], Int, Int)): Stream[(List[Int], Int, Int)] = for {
      
      i <- (0 until 10).toStream
      newSum = digits._2 + (1 to i).product
      newNumber = digits._3 * 10 + i
      //don't use new values if not viable
      if (newSum == newNumber || 10*newNumber <= newSum + maxAdd)
    } yield (i :: digits._1, newSum, newNumber)

    def addLevel(digits: Stream[(List[Int], Int, Int)]): Stream[(List[Int], Int, Int)] = {
      val added = for {
        item <- digits
      } yield addDigit(item)
      val lolo = added.flatten
      if (lolo.isEmpty) digits else digits #::: addLevel(lolo)
    }
    //addLevel(addDigit((List(), 0, 0)).drop(1)).filter((p) => (p._2 == p._3 && p._2 != 1)).toList.map(_._3).sum
    //addLevel(addDigit((List(), 0, 0)).drop(1)).filter((p) => (p._2 == p._3 && p._2 != 1))
    //addLevel(Stream((List(1), 1, 1))).drop(3).filter((p) => p._2 == p._3).map(_._3)
    addLevel(addDigit((List(), 0, 0)).drop(1)).drop(2).filter((p) => p._2 == p._3).map(_._3).sum
  }
  
  def problem35 = {
    val a = Stream.range(1, 10000000)
    //val a = List.range(1, 10000000)
    val pr = Primes()
    def circle(num:Int, length: Long):Stream[Long] = {
      val tenPower = Math.round(Math.pow(10, length))
      def _inner(num2:Long):Stream[Long] = {
          val v1 = num2 % 10
          val newNum0 = num2/10
          //val tens = Math.round(Math.pow(10, Math.floor(Math.log10(num2))))
          val newNum =  tenPower*v1+newNum0
          if(newNum==num) Stream.Empty else newNum #:: _inner(newNum)
      }
      num #:: _inner(num)
    }
    //a.map((x:Int) => circle(x,Math.round(Math.floor(Math.log10(x))))).filter { _.forall { pr.isPrime(_) } }
    a.filter { (p:Int) => circle(p,Math.round(Math.floor(Math.log10(p)))).forall(pr.isPrime(_))}
  }
  
}