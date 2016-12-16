package dev.adventofcode

import dev.adventofcode.Problem22._
import org.scalatest.{FlatSpec, Matchers}

class Problem22Test extends FlatSpec with Matchers {

  behavior of "magic missile"

  it should "instantly do 4 damage and cost 53 mana" in {
    val player = Player(50, 0, 100, Set.empty)
    val boss = Boss(50, 10)

    val (playerAfterCast, bossAfterCast) = MagicMissile.cast(player, boss)

    playerAfterCast shouldEqual player.copy(mana = player.mana - 53)
    bossAfterCast shouldEqual boss.copy(hp = boss.hp - 4)
  }

  behavior of "drain"

  it should "instantly do 2 damage, heal the player for 2 hp, and cost 73 mana" in {
    val player = Player(50, 0, 100, Set.empty)
    val boss = Boss(50, 10)

    val (playerAfterCast, bossAfterCast) = Drain.cast(player, boss)

    playerAfterCast shouldEqual player.copy(mana = player.mana - 73, hp = player.hp + 2)
    bossAfterCast shouldEqual boss.copy(hp = boss.hp - 2)
  }

  behavior of "shield"

  it should "increase player armor by 7 while active and cost 113 mana" in {
    val player = Player(50, 0, 200, Set.empty)
    val boss = Boss(50, 10)

    val shieldTurn2 = Shield(2)

    val (playerAfterCast, bossAfterCast) = shieldTurn2.cast(player, boss)

    playerAfterCast shouldEqual player.copy(armor = 7, mana = player.mana - 113, effects = Set(shieldTurn2))
    bossAfterCast shouldEqual boss

    val (playerAfterTick1, bossAfterTick1) = shieldTurn2.tick(playerAfterCast, bossAfterCast)
    playerAfterTick1 shouldEqual playerAfterCast.copy(armor = 7, effects = Set(shieldTurn2.copy(turns = 1)))
    bossAfterTick1 shouldEqual boss

    val (playerAfterTick2, bossAfterTick2) = playerAfterTick1.effects.head.tick(playerAfterTick1, bossAfterTick1)
    playerAfterTick2 shouldEqual playerAfterTick1.copy(armor = 7, effects = Set(shieldTurn2.copy(turns = 0)))
    bossAfterTick2 shouldEqual boss

    val (playerAfterTick3, bossAfterTick3) = playerAfterTick2.effects.head.tick(playerAfterTick2, bossAfterTick2)
    playerAfterTick3 shouldEqual playerAfterTick2.copy(armor = 0, effects = Set.empty)
    bossAfterTick3 shouldEqual boss
  }

  behavior of "poison"

  it should "deal 3 damage while active and cost 173 mana" in {
    val player = Player(50, 0, 200, Set.empty)
    val boss = Boss(50, 10)

    val poisonTurn2 = Poison(2)

    val (playerAfterCast, bossAfterCast) = poisonTurn2.cast(player, boss)

    playerAfterCast shouldEqual player.copy(mana = player.mana - 173, effects = Set(poisonTurn2))
    bossAfterCast shouldEqual boss

    val (playerAfterTick1, bossAfterTick1) = poisonTurn2.tick(playerAfterCast, bossAfterCast)
    playerAfterTick1 shouldEqual playerAfterCast.copy(effects = Set(poisonTurn2.copy(turns = 1)))
    bossAfterTick1 shouldEqual bossAfterCast.copy(hp = bossAfterCast.hp - 3)

    val (playerAfterTick2, bossAfterTick2) = playerAfterTick1.effects.head.tick(playerAfterTick1, bossAfterTick1)
    playerAfterTick2 shouldEqual playerAfterTick1.copy(effects = Set(poisonTurn2.copy(turns = 0)))
    bossAfterTick2 shouldEqual bossAfterTick1.copy(hp = bossAfterTick1.hp - 3)

    val (playerAfterTick3, bossAfterTick3) = playerAfterTick2.effects.head.tick(playerAfterTick2, bossAfterTick2)
    playerAfterTick3 shouldEqual playerAfterTick2.copy(effects = Set.empty)
    bossAfterTick3 shouldEqual bossAfterTick2
  }

  behavior of "recharge"

  it should "grant 101 mana while active and cost 229 mana" in {
    val player = Player(50, 0, 200, Set.empty)
    val boss = Boss(50, 10)

    val rechargeTurn2 = Recharge(2)

    val (playerAfterCast, bossAfterCast) = rechargeTurn2.cast(player, boss)

    playerAfterCast shouldEqual player.copy(mana = player.mana - 229, effects = Set(rechargeTurn2))
    bossAfterCast shouldEqual boss

    val (playerAfterTick1, bossAfterTick1) = rechargeTurn2.tick(playerAfterCast, bossAfterCast)
    playerAfterTick1 shouldEqual playerAfterCast.copy(mana = playerAfterCast.mana + 101, effects = Set(rechargeTurn2.copy(turns = 1)))
    bossAfterTick1 shouldEqual boss

    val (playerAfterTick2, bossAfterTick2) = playerAfterTick1.effects.head.tick(playerAfterTick1, bossAfterTick1)
    playerAfterTick2 shouldEqual playerAfterTick1.copy(mana = playerAfterTick1.mana + 101, effects = Set(rechargeTurn2.copy(turns = 0)))
    bossAfterTick2 shouldEqual boss

    val (playerAfterTick3, bossAfterTick3) = playerAfterTick2.effects.head.tick(playerAfterTick2, bossAfterTick2)
    playerAfterTick3 shouldEqual playerAfterTick2.copy(effects = Set.empty)
    bossAfterTick3 shouldEqual boss
  }

  behavior of "examples"

  it should "match the outcome of the first example" in {
    val player = Player(10, 0, 250, Set.empty)
    val boss = Boss(13, 8)

    val (playerAfterTurn1, bossAfterTurn1) = new Poison().cast(player, boss)
    playerAfterTurn1 shouldEqual Player(10, 0, 77, Set(Poison(6)))
    bossAfterTurn1 shouldEqual Boss(13, 8)

    val (playerAfterTurn2Effects, bossAfterTurn2) = Problem22.applyEffects(playerAfterTurn1, bossAfterTurn1)
    val playerAfterTurn2 = boss.attack(playerAfterTurn2Effects)
    playerAfterTurn2 shouldEqual Player(2, 0, 77, Set(Poison(5)))
    bossAfterTurn2 shouldEqual Boss(10, 8)

    val (playerAfterTurn3Effects, bossAfterTurn3Effects) = Problem22.applyEffects(playerAfterTurn2, bossAfterTurn2)
    val (playerAfterTurn3, bossAfterTurn3) = MagicMissile.cast(playerAfterTurn3Effects, bossAfterTurn3Effects)
    playerAfterTurn3 shouldEqual Player(2, 0, 24, Set(Poison(4)))
    bossAfterTurn3 shouldEqual Boss(3, 8)

    val (playerAfterTurn4Effects, bossAfterTurn4Effects) = Problem22.applyEffects(playerAfterTurn3, bossAfterTurn3)
    bossAfterTurn4Effects.hp should be <= 0
  }

  it should "match the outcome of the second example" in {
    // -- Player turn --
    // - Player has 10 hit points, 0 armor, 250 mana
    // - Boss has 14 hit points
    // Player casts Recharge.
    val player = Player(10, 0, 250, Set.empty)
    val boss = Boss(14, 8)
    val (playerAfterTurn1, bossAfterTurn1) = new Recharge().cast(player, boss)

    // -- Boss turn --
    // - Player has 10 hit points, 0 armor, 21 mana
    // - Boss has 14 hit points
    // Recharge provides 101 mana; its timer is now 4.
    // Boss attacks for 8 damage!
    playerAfterTurn1 shouldEqual Player(10, 0, 21, Set(Recharge(5)))
    bossAfterTurn1 shouldEqual Boss(14, 8)
    val (playerAfterTurn2Effects, bossAfterTurn2) = Problem22.applyEffects(playerAfterTurn1, bossAfterTurn1)
    val playerAfterTurn2 = boss.attack(playerAfterTurn2Effects)

    // -- Player turn --
    // - Player has 2 hit points, 0 armor, 122 mana
    // - Boss has 14 hit points
    // Recharge provides 101 mana; its timer is now 3.
    // Player casts Shield, increasing armor by 7.
    playerAfterTurn2 shouldEqual Player(2, 0, 122, Set(Recharge(4)))
    bossAfterTurn2 shouldEqual Boss(14, 8)
    val (playerAfterTurn3Effects, bossAfterTurn3Effects) = Problem22.applyEffects(playerAfterTurn2, bossAfterTurn2)
    val (playerAfterTurn3, bossAfterTurn3) = new Shield().cast(playerAfterTurn3Effects, bossAfterTurn3Effects)

    // -- Boss turn --
    // - Player has 2 hit points, 7 armor, 110 mana
    // - Boss has 14 hit points
    // Shield's timer is now 5.
    // Recharge provides 101 mana; its timer is now 2.
    // Boss attacks for 8 - 7 = 1 damage!
    playerAfterTurn3 shouldEqual Player(2, 7, 110, Set(Recharge(3), Shield(6)))
    bossAfterTurn3 shouldEqual Boss(14, 8)
    val (playerAfterTurn4Effects, bossAfterTurn4) = Problem22.applyEffects(playerAfterTurn3, bossAfterTurn3)
    val playerAfterTurn4 = boss.attack(playerAfterTurn4Effects)


    // -- Player turn --
    // - Player has 1 hit point, 7 armor, 211 mana
    // - Boss has 14 hit points
    // Shield's timer is now 4.
    // Recharge provides 101 mana; its timer is now 1.
    // Player casts Drain, dealing 2 damage, and healing 2 hit points.
    playerAfterTurn4 shouldEqual Player(1, 7, 211, Set(Recharge(2), Shield(5)))
    bossAfterTurn4 shouldEqual Boss(14, 8)
    val (playerAfterTurn5Effects, bossAfterTurn5Effects) = Problem22.applyEffects(playerAfterTurn4, bossAfterTurn4)
    val (playerAfterTurn5, bossAfterTurn5) = Drain.cast(playerAfterTurn5Effects, bossAfterTurn5Effects)

    //   -- Boss turn --
    //   - Player has 3 hit points, 7 armor, 239 mana
    //   - Boss has 12 hit points
    // Shield's timer is now 3.
    // Recharge provides 101 mana; its timer is now 0.
    // Recharge wears off.
    //   Boss attacks for 8 - 7 = 1 damage!
    playerAfterTurn5 shouldEqual Player(3, 7, 239, Set(Recharge(1), Shield(4)))
    bossAfterTurn5 shouldEqual Boss(12, 8)
    val (playerAfterTurn6Effects, bossAfterTurn6) = Problem22.applyEffects(playerAfterTurn5, bossAfterTurn5)
    val playerAfterTurn6 = boss.attack(playerAfterTurn6Effects)

    //   -- Player turn --
    //   - Player has 2 hit points, 7 armor, 340 mana
    //   - Boss has 12 hit points
    // Shield's timer is now 2.
    // Player casts Poison.
    playerAfterTurn6 shouldEqual Player(2, 7, 340, Set(Recharge(0), Shield(3)))
    bossAfterTurn6 shouldEqual Boss(12, 8)
    val (playerAfterTurn7Effects, bossAfterTurn7Effects) = Problem22.applyEffects(playerAfterTurn6, bossAfterTurn6)
    val (playerAfterTurn7, bossAfterTurn7) = new Poison().cast(playerAfterTurn7Effects, bossAfterTurn7Effects)

    //   -- Boss turn --
    //   - Player has 2 hit points, 7 armor, 167 mana
    //   - Boss has 12 hit points
    // Shield's timer is now 1.
    // Poison deals 3 damage; its timer is now 5.
    // Boss attacks for 8 - 7 = 1 damage!
    playerAfterTurn7 shouldEqual Player(2, 7, 167, Set(Shield(2), Poison(6)))
    bossAfterTurn7 shouldEqual Boss(12, 8)
    val (playerAfterTurn8Effects, bossAfterTurn8) = Problem22.applyEffects(playerAfterTurn7, bossAfterTurn7)
    val playerAfterTurn8 = boss.attack(playerAfterTurn8Effects)

    //   -- Player turn --
    //   - Player has 1 hit point, 7 armor, 167 mana
    //   - Boss has 9 hit points
    // Shield's timer is now 0.
    // Shield wears off, decreasing armor by 7.
    // Poison deals 3 damage; its timer is now 4.
    // Player casts Magic Missile, dealing 4 damage.
    playerAfterTurn8 shouldEqual Player(1, 7, 167, Set(Shield(1), Poison(5)))
    bossAfterTurn8 shouldEqual Boss(9, 8)
    val (playerAfterTurn9Effects, bossAfterTurn9Effects) = Problem22.applyEffects(playerAfterTurn8, bossAfterTurn8)
    val (playerAfterTurn9, bossAfterTurn9) = MagicMissile.cast(playerAfterTurn9Effects, bossAfterTurn9Effects)

    // -- Boss turn --
    //   - Player has 1 hit point, 0 armor, 114 mana
    //   - Boss has 2 hit points
    // Poison deals 3 damage. This kills the boss, and the player wins.
    playerAfterTurn9 shouldEqual Player(1, 7, 114, Set(Shield(0), Poison(4)))
    bossAfterTurn9 shouldEqual Boss(2, 8)
    val (playerAfterTurn10Effects, bossAfterTurn10Effects) = Problem22.applyEffects(playerAfterTurn9, bossAfterTurn9)
    bossAfterTurn10Effects.hp should be <= 0
  }
}
