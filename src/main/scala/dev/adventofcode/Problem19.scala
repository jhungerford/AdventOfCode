package dev.adventofcode

import scala.util.Random

object Problem19 {

  val lineRegex = """([a-zA-Z]+) => ([a-zA-Z]+)""".r
  def parseBackwardsReplacements(lines: List[String]): Map[String, String] = {
    lines.foldLeft(Map.empty[String, String]) { (map, line) =>
      line match {
        case lineRegex(from, to) => map + (to -> from)
        case _ => map
      }
    }
  }

  def randomStepsToMolecule(finalMolecule: String, replacements: Map[String, String]): Int = {
    // Solution stolen from this reddit post:
    // https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4cu5b
    // Find _a_ solution from toMolecule to 'e'.  Sort the replacements into a random order.
    // Replace as many sub-molecules as possible until we either reach 'e' or do nothing

    // Shuffle so the order will be different each time this method is called
    val replacementKeys = Random.shuffle(replacements.keySet.toList)

    def replaceInOrder(molecule: String): (Int, String) = {
      replacementKeys.foldLeft((0, molecule)) {
        case ((steps, moleculeSoFar), replacementKey) =>
          moleculeSoFar.indexOf(replacementKey) match {
            case -1 => (steps, moleculeSoFar)
            case index =>
              val replacedMolecule = moleculeSoFar.substring(0, index) + replacements(replacementKey) + moleculeSoFar.substring(index + replacementKey.length)
              (steps + 1, replacedMolecule)
          }
      }
    }

    def totalSteps(molecule: String): Option[Int] = {
      val (steps, newMolecule) = replaceInOrder(molecule)

      if (newMolecule == "e") {
        Some(steps)
      } else if (newMolecule == molecule) {
        None
      } else {
        totalSteps(newMolecule).map(additionalSteps => additionalSteps + steps)
      }
    }

    totalSteps(finalMolecule) match {
      case None => randomStepsToMolecule(finalMolecule, replacements)
      case Some(n) => n
    }
  }

  def main(args: Array[String]) {
    val replacements = parseBackwardsReplacements(List(
      "Al => ThF",
      "Al => ThRnFAr",
      "B => BCa",
      "B => TiB",
      "B => TiRnFAr",
      "Ca => CaCa",
      "Ca => PB",
      "Ca => PRnFAr",
      "Ca => SiRnFYFAr",
      "Ca => SiRnMgAr",
      "Ca => SiTh",
      "F => CaF",
      "F => PMg",
      "F => SiAl",
      "H => CRnAlAr",
      "H => CRnFYFYFAr",
      "H => CRnFYMgAr",
      "H => CRnMgYFAr",
      "H => HCa",
      "H => NRnFYFAr",
      "H => NRnMgAr",
      "H => NTh",
      "H => OB",
      "H => ORnFAr",
      "Mg => BF",
      "Mg => TiMg",
      "N => CRnFAr",
      "N => HSi",
      "O => CRnFYFAr",
      "O => CRnMgAr",
      "O => HP",
      "O => NRnFAr",
      "O => OTi",
      "P => CaP",
      "P => PTi",
      "P => SiRnFAr",
      "Si => CaSi",
      "Th => ThCa",
      "Ti => BP",
      "Ti => TiTi",
      "e => HF",
      "e => NAl",
      "e => OMg"
    ))

    val molecule = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"

    val steps = randomStepsToMolecule(molecule, replacements)
    System.out.println(s"$steps steps to make the molecule")
  }

}
