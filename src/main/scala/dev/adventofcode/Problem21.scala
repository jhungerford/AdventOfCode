package dev.adventofcode

object Problem21 {

  val emptyItem: Item = Item("Empty", 0, 0, 0)

  case class Item(name: String, cost: Int, damage: Int, armor: Int)

  case class Monster(hp: Int, damage: Int, armor: Int)

  case class Player(hp: Int, weapon: Item, mail: Item, ring1: Item, ring2: Item) {
    lazy val damage: Int = weapon.damage + mail.damage + ring1.damage + ring2.damage
    lazy val armor: Int = weapon.armor + mail.armor + ring1.armor + ring2.armor
    lazy val cost: Int = weapon.cost + mail.cost + ring1.cost + ring2.cost
  }

  def playerWins(player: Player, monster: Monster): Boolean = {
    val playerDamage = Math.max(player.damage - monster.armor, 1)
    val monsterDamage = Math.max(monster.damage - player.armor, 1)

    val playerTurns = Math.ceil(monster.hp.toDouble / playerDamage)
    val monsterTurns = Math.ceil(player.hp.toDouble / monsterDamage)

    playerTurns <= monsterTurns
  }

  def main(args: Array[String]) {
    val weapons = Set(
      Item("Dagger", 8, 4, 0),
      Item("Shortsword", 10, 5, 0),
      Item("Warhammer", 25, 6, 0),
      Item("Longsword", 40, 7, 0),
      Item("Greataxe", 74, 8, 0)
    )

    val armor = Set(
      Item("Leather", 13, 0, 1),
      Item("Chainmail", 31, 0, 2),
      Item("Splintmail", 53, 0, 3),
      Item("Bandedmail", 75, 0, 4),
      Item("Platemail", 102, 0, 5)
    )

    val rings = Set(
      Item("Damage +1", 25, 1, 0),
      Item("Damage +2", 50, 2, 0),
      Item("Damage +3", 100, 3, 0),
      Item("Defense +1", 20, 0, 1),
      Item("Defense +2", 40, 0, 2),
      Item("Defense +3", 80, 0, 3)
    )

    val monster = Monster(104, 8, 1)

    val players = for {
      playerWeapon <- weapons
      playerArmor <- armor + emptyItem
      playerRing1 <- rings + emptyItem
      playerRing2 <- rings - playerRing1 + emptyItem
    } yield Player(100, playerWeapon, playerArmor, playerRing1, playerRing2)

    val lowestCost: Int = players.filter {
      player => playerWins(player, monster)
    }.map {
      player => player.cost
    }.toList.sorted.head

    System.out.println(s"Part 1 - least amount of gold to win: $lowestCost")

    val highestCost: Int = players.filter {
      player => ! playerWins(player, monster)
    }.map {
      player => player.cost
    }.toList.sorted.last

    System.out.println(s"Part 2 - most amount of gold to lose: $highestCost")
  }
}
