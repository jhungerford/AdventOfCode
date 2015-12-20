package dev.adventofcode

object Problem19 {

  def indexesOf(find: String, in: String, list: List[Int] = List.empty): List[Int] = {
    val from = list.lastOption.getOrElse(-1) + 1
    in.indexOf(find, from) match {
      case -1 => list
      case index => indexesOf(find, in, list :+ index)
    }
  }

  def generateReplacements(molecule: String, replacements: Map[String, List[String]]): Set[String] = {
    val newMoleculeList = for {
      (oldAtom, newAtoms) <- replacements
      index <- indexesOf(oldAtom, molecule)
      newAtom <- newAtoms
    } yield molecule.substring(0, index) + molecule.substring(index).replaceFirst(oldAtom, newAtom)

    newMoleculeList.toSet
  }

  val lineRegex = """([a-zA-Z]+) => ([a-zA-Z]+)""".r
  def parseReplacements(lines: List[String]) = {
    lines.foldLeft(Map.empty[String, List[String]]) { (map, line) =>
      line match {
        case lineRegex(from, to) => map + (from -> (map.getOrElse(from, List.empty[String]) :+ to))
        case _ => map
      }
    }
  }

  // Note: stepsToMolecule doesn't terminate if the molecule isn't possible.
  def stepsToMolecule(toMolecule: String, replacements: Map[String, List[String]]): Int = {
    Stream.iterate(Set("e")) { molecules =>
      molecules.flatMap { molecule =>
        generateReplacements(molecule, replacements)
      }
    }.indexWhere { molecules => molecules.contains(toMolecule) }
  }

  def main(args: Array[String]) {
    val replacements = parseReplacements(List(
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

    val newMolecules = Problem19.generateReplacements(molecule, replacements)

    System.out.println(s"${newMolecules.size} different ways to do one replacement")

    val steps = stepsToMolecule(molecule, replacements)
    System.out.println(s"$steps steps to make the molecule")
  }

}
