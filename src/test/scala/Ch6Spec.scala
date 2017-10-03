package fpclub6

import fpclub6.Ch6._
import org.scalatest.{FlatSpec, Matchers}

class Ch6Spec extends FlatSpec with Matchers {
  "Candy Machine addCoin" should "always increment the coin" in {
    addCoin.run(Machine(true,0,0))._2 should be(Machine(true,0,1))
    addCoin.run(Machine(false,0,0))._2 should be(Machine(false,0,1))
    addCoin.run(Machine(true,0,-5))._2 should be(Machine(true,0,-4))
    addCoin.run(Machine(false,0,-5))._2 should be(Machine(false,0,-4))
    addCoin.run(Machine(true,44,0))._2 should be(Machine(true,44,1))
  }

  "Candy Machine dispenseCandy" should "always decrement the candy" in {
    dispenseCandy.run(Machine(true,42,0))._2 should be(Machine(true,41,0))
  }

  "Locked Candy machine" should "do nothing on Turn" in {
    doInput(Turn).run(Machine(true, 42, 15))._2 should be(Machine(true,42,15))
  }

  "Locked Candy machine" should "unlock and increment coins on Coin" in {
    val coin = 16
    doInput(Coin).run(Machine(true, 42, coin))._2 should be(Machine(false,42,coin+1))
  }

  "Unlocked Candy machine" should "do nothing on Coin" in {
    val coin = 16
    doInput(Coin).run(Machine(false, 42, coin))._2 should be(Machine(false,42,coin))
  }

  "Candy Machine" should "correctly process the example from textbook" in {
    val machine = Machine(true, candies = 5, coins = 10)
    val (r, _) = simulateMachine(List(
          Coin, Turn,
          Coin, Turn,
          Coin, Turn,
          Coin, Turn
        )).run(machine)

    r should be((14,1))
  }
}
