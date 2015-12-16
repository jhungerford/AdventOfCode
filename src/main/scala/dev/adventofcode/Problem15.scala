package dev.adventofcode

import scala.collection.immutable.IndexedSeq

object Problem15 {

  case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)
  val sprinkles = Ingredient(5, -1, 0, 0, 5)
  val peanutButter = Ingredient(-1, 3, 0, 0, 1)
  val frosting = Ingredient(0, -1, 4, 0, 6)
  val sugar = Ingredient(-1, 0, 0, 2, 8)

  def score(numSprinkles: Int, numPeanutButter: Int, numFrosting: Int, numSugar: Int): Int = {
    val capacity = numSprinkles * sprinkles.capacity + numPeanutButter * peanutButter.capacity + numFrosting * frosting.capacity + numSugar * sugar.capacity
    val durability = numSprinkles * sprinkles.durability + numPeanutButter * peanutButter.durability + numFrosting * frosting.durability + numSugar * sugar.durability
    val flavor = numSprinkles * sprinkles.flavor + numPeanutButter * peanutButter.flavor + numFrosting * frosting.flavor + numSugar * sugar.flavor
    val texture = numSprinkles * sprinkles.texture + numPeanutButter * peanutButter.texture + numFrosting * frosting.texture + numSugar * sugar.texture

    if (capacity < 0 || durability < 0 || flavor < 0 || texture < 0) {
      0
    } else {
      capacity * durability * flavor * texture
    }
  }

  def scoreWith500Calories(numSprinkles: Int, numPeanutButter: Int, numFrosting: Int, numSugar: Int): Int = {
    val calories = numSprinkles * sprinkles.calories + numPeanutButter * peanutButter.calories + numFrosting * frosting.calories + numSugar * sugar.calories
    if (calories != 500) {
      return 0
    }

    val capacity = numSprinkles * sprinkles.capacity + numPeanutButter * peanutButter.capacity + numFrosting * frosting.capacity + numSugar * sugar.capacity
    val durability = numSprinkles * sprinkles.durability + numPeanutButter * peanutButter.durability + numFrosting * frosting.durability + numSugar * sugar.durability
    val flavor = numSprinkles * sprinkles.flavor + numPeanutButter * peanutButter.flavor + numFrosting * frosting.flavor + numSugar * sugar.flavor
    val texture = numSprinkles * sprinkles.texture + numPeanutButter * peanutButter.texture + numFrosting * frosting.texture + numSugar * sugar.texture

    if (capacity < 0 || durability < 0 || flavor < 0 || texture < 0) {
      0
    } else {
      capacity * durability * flavor * texture
    }
  }

  def main(args: Array[String]) {
    val scores: IndexedSeq[Int] = for {
      numSprinkles <- 0 to 100
      numPeanutButter <- 0 to (100 - numSprinkles)
      numFrosting <- 0 to (100 - numSprinkles - numPeanutButter)
    } yield score(numSprinkles, numPeanutButter, numFrosting, 100 - numSprinkles - numPeanutButter - numFrosting)


    System.out.println(s"Max score: ${scores.max}")

    val scores500Calories: IndexedSeq[Int] = for {
      numSprinkles <- 0 to 100
      numPeanutButter <- 0 to (100 - numSprinkles)
      numFrosting <- 0 to (100 - numSprinkles - numPeanutButter)
    } yield scoreWith500Calories(numSprinkles, numPeanutButter, numFrosting, 100 - numSprinkles - numPeanutButter - numFrosting)

    System.out.println(s"Max score with 500 calories: ${scores500Calories.max}")
  }

}
