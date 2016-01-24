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

  def heuristicDistance(from: String, to: String): Int = {
//    val fromUpper = from.filter { c => c.isUpper }
//    val toUpper = to.filter { c => c.isUpper }

    // https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm - DP approach to Levenshtein distance
    val d = Array.ofDim[Int](from.length + 1, to.length + 1)
    (0 to from.length).foreach { i => d(i)(0) = i }
    (0 to to.length).foreach { j => d(0)(j) = j }

    (1 to to.length).foreach { j =>
      (1 to from.length).foreach { i =>
        if (from.charAt(i-1) == to.charAt(j-1)) {
          d(i)(j) = d(i-1)(j-1)

        } else {
          val deletion = d(i-1)(j) + 1
          val insertion = d(i)(j-1) + 1
          val substitution = d(i-1)(j-1) + 1

          d(i)(j) = Math.min(Math.min(deletion, insertion), substitution)
        }
      }
    }

    d(from.length)(to.length)
  }

  // Note: stepsToMolecule doesn't terminate if the molecule isn't possible.
  def stepsToMolecule(toMolecule: String, replacements: Map[String, List[String]]): Int = {
    def aStar(open: Set[String], gScore: Map[String, Int], fScore: Map[String, Int]): Int = {
      if (open.isEmpty) {
        -1
      } else {
        val current = open.minBy { score => fScore(score) } // Lowest fScore
        System.out.println(s"fScore: ${fScore(current)}\tgScore: ${gScore(current)}\topen: ${open.size}\tmolecule: $current")

        if (current == toMolecule) {
          gScore(current)
        } else {
          val neighbors = generateReplacements(current, replacements)

          val (openResult, gScoreResult, fScoreResult) = neighbors.foldLeft((open - current, gScore, fScore)) {
            case ((newOpen, newGScore, newFScore), neighbor) =>
              val tentativeGScore = gScore(current) + 1

              if (open.contains(neighbor) && tentativeGScore >= newGScore(neighbor)) {
                (newOpen, newGScore, newFScore) // This path is longer, ignore it.
              } else {
                (
                  newOpen + neighbor,
                  newGScore + (neighbor -> tentativeGScore),
                  newFScore + (neighbor -> (tentativeGScore + heuristicDistance(neighbor, toMolecule)))
                )
              }
          }

          aStar(openResult, gScoreResult, fScoreResult)
        }
      }
    }

    val startMolecule = "e"

    val open = Set(startMolecule)
    val gScore = Map(startMolecule -> 0)
    val fScore = Map(startMolecule -> heuristicDistance(startMolecule, toMolecule))

    aStar(open, gScore, fScore)
  }

  def main(args: Array[String]) {
    val replacements = parseReplacements(List(
      "L => TF",
      "L => TRFA",
      "B => BC",
      "B => IB",
      "B => IRFA",
      "C => CC",
      "C => PB",
      "C => PRFA",
      "C => SRFYFA",
      "C => SRMA",
      "C => ST",
      "F => CF",
      "F => PM",
      "F => SL",
      "H => CRLA",
      "H => CRFYFYFA",
      "H => CRFYMA",
      "H => CRMYFA",
      "H => HC",
      "H => NRFYFA",
      "H => NRMA",
      "H => NT",
      "H => OB",
      "H => ORFA",
      "M => BF",
      "M => IM",
      "N => CRFA",
      "N => HS",
      "O => CRFYFA",
      "O => CRMA",
      "O => HP",
      "O => NRFA",
      "O => OI",
      "P => CP",
      "P => PI",
      "P => SRFA",
      "S => CS",
      "T => TC",
      "I => BP",
      "I => II",
      "e => HF",
      "e => NL",
      "e => OM"
    ))

    val molecule = "CRCSRBSRFAIBPIIBFAPBCSTSRIBPBPMACSRIMACSTCSRFARSRFAIIBFACCSRSTCCSRMAFYSRFYCFASTCSTPBPIMACPRSLAPBCCSRFYSTCRFAACCSRPBSRFAMYCCCCSTCCSLACCSRPBSLABCCCCSTCPBSTPBPBCSRFYFASTCSRFABCCSRFYFASTCPBSTCSRPMARFAPIBCPRFACCCCSRCCSRFYFAFABCSTFATSTSRIRPMAFACSTCPBCSRBFACCPRCCPMASRFYFACSTRPBPMA"

    val newMolecules = Problem19.generateReplacements(molecule, replacements)

    System.out.println(s"${newMolecules.size} different ways to do one replacement")

    val steps = stepsToMolecule(molecule, replacements)
    System.out.println(s"$steps steps to make the molecule")
  }

}
