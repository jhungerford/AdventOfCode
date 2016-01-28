package dev.adventofcode

object Problem22 {

  case class Player(hp: Int, armor: Int, mana: Int, effects: Set[Effect])

  case class Boss(hp: Int, damage: Int) {
    def attack(player: Player): Player = {
      player.copy(hp = player.hp - (damage - player.armor))
    }
  }

  trait Spell {
    def cost: Int
    def cast(player: Player, boss: Boss): (Player, Boss)
  }

  trait Effect extends Spell {
    def tick(player: Player, boss: Boss): (Player, Boss)
  }

  // Magic missle costs 53 mana - it instantly does 4 damage
  object MagicMissile extends Spell {
    val cost = 53

    override def cast(player: Player, boss: Boss): (Player, Boss) = (
      player.copy(mana = player.mana - cost),
      boss.copy(hp = boss.hp - 4))
  }

  // Drain costs 73 mana.  It instantly does 2 damage and heals you for 2hp
  object Drain extends Spell {
    val cost = 73

    override def cast(player: Player, boss: Boss): (Player, Boss) = (
      player.copy(mana = player.mana - cost, hp = player.hp + 2),
      boss.copy(hp = boss.hp - 2))
  }

  // Shield costs 113 mana.  It starts an effect that lasts for 6 turns.
  // While active, your armor is increased by 7
  case class Shield(turns: Int = 6) extends Effect {
    val cost = 113

    override def cast(player: Player, boss: Boss): (Player, Boss) = (
      player.copy(mana = player.mana - cost, effects = player.effects + this),
      boss)

    override def tick(player: Player, boss: Boss): (Player, Boss) = {
      turns match {
        case 0 => (player.copy(armor = 0, effects = player.effects - this), boss)
        case _ => (player.copy(armor = 7, effects = player.effects - this + copy(turns = turns - 1)), boss)
      }
    }
  }

  // Poison costs 173 mana.  It starts an effect that lasts for 6 turns.
  // At the start of each turn while it is active, it deals the boss 3 damage.
  case class Poison(turns: Int = 6) extends Effect {
    val cost = 173

    override def cast(player: Player, boss: Boss): (Player, Boss) = (
      player.copy(mana = player.mana - cost, effects = player.effects + this),
      boss)

    override def tick(player: Player, boss: Boss): (Player, Boss) = {
      turns match {
        case 0 => (player.copy(effects = player.effects - this), boss)
        case _ => (player.copy(effects = player.effects - this + copy(turns = turns - 1)), boss.copy(hp = boss.hp - 3))
      }
    }
  }

  // Recharge costs 229 mana.  It starts an effect that lasts for 5 turns.
  // At the start of each turn while it is active, it gives you 101 new mana.
  case class Recharge(turns: Int = 5) extends Effect {
    val cost = 229

    override def cast(player: Player, boss: Boss): (Player, Boss) = (
      player.copy(mana = player.mana - cost, effects = player.effects + this),
      boss)

    override def tick(player: Player, boss: Boss): (Player, Boss) = {
      turns match {
        case 0 => (player.copy(effects = player.effects - this), boss)
        case _ => (player.copy(mana = player.mana + 101, effects = player.effects - this + copy(turns = turns - 1)), boss)
      }
    }
  }

  case class Outcome(playerWon: Boolean, mana: Int)

  val allSpells: Set[Spell] = Set(MagicMissile, Drain, new Shield, new Poison, new Recharge)

  def playerTurn(player: Player, boss: Boss, manaSoFar: Int): List[Outcome] = {
    if (manaSoFar > 1000) {
      List.empty
    } else {
      // Apply effects
      val (playerAfterEffects, bossAfterEffects) = applyEffects(player, boss)

      // either the player died, the boss died, or we need to enumerate spells
      if (playerAfterEffects.hp <= 0) {
        List(Outcome(playerWon = false, manaSoFar))

      } else if (bossAfterEffects.hp <= 0) {
        List(Outcome(playerWon = true, manaSoFar))

      } else {
        // Cast all of the spells the player still has mana for
        val availableSpells = allSpells.filterNot { spell =>
          playerAfterEffects.effects.exists { activeEffect => spell.getClass == activeEffect.getClass }
        }.filter(spell => spell.cost <= player.mana)

        availableSpells.toList.flatMap { spell =>
          val (playerAfterSpell, bossAfterSpell) = spell.cast(playerAfterEffects, bossAfterEffects)

          if (playerAfterSpell.hp <= 0) {
            List(Outcome(playerWon = false, manaSoFar + spell.cost))

          } else if (bossAfterSpell.hp <= 0) {
            List(Outcome(playerWon = true, manaSoFar + spell.cost))

          } else {
            bossTurn(playerAfterSpell, bossAfterSpell, manaSoFar + spell.cost)
          }
        }
      }
    }
  }

  def bossTurn(player: Player, boss: Boss, manaSoFar: Int): List[Outcome] = {
    // Apply attacks
    val (playerAfterEffects, bossAfterEffects) = applyEffects(player, boss)

    if (playerAfterEffects.hp <= 0) {
      List(Outcome(playerWon = false, manaSoFar))

    } else if (bossAfterEffects.hp <= 0) {
      List(Outcome(playerWon = true, manaSoFar))

    } else {
      // Attack
      val playerAfterAttack = boss.attack(playerAfterEffects)

      if (playerAfterAttack.hp <= 0) {
        List(Outcome(playerWon = false, manaSoFar))
      } else {
        playerTurn(player, boss, manaSoFar)
      }
    }
  }

  // Effects apply at the start of both the player's and the boss' turns
  // You cannot cast a spell that would start an already active effect.
  // Effects can be started on the same turn they end
  def applyEffects(player: Player, boss: Boss): (Player, Boss) = {
    player.effects.foldLeft((player, boss)) {
      case ((newPlayer, newBoss), effect) => effect.tick(newPlayer, newBoss)
    }
  }

  def main(args: Array[String]) {
    val player = Player(50, 0, 500, Set.empty)
    val boss = Boss(51, 9)

    // Player goes first - build out the game tree,
    // pruning branches where the player either runs out of mana or loses

    val outcomes: List[Outcome] = playerTurn(player, boss, 0)
    val playerWonManas = outcomes.flatMap {
      case Outcome(true, totalMana) => Some(totalMana)
      case _ => None
    }

    System.out.println(s"Least amount of mana spent to win the fight: ${playerWonManas.sorted.head}")
  }
}
