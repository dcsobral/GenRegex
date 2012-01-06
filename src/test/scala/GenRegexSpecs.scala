import org.specs2._
import specification.gen._
import matcher.DataTables
import execute.Result
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object RegexGen {
  val metacharacters = ".|?*+{}[]()".toSet
  
  def regexp: Gen[String] = alt
  def alt: Gen[String] = Gen listOf reps map (_ mkString "|")
  def reps: Gen[String] = Gen oneOf (zeroOrOne, zeroOrMore, oneOrMore)
  def zeroOrOne: Gen[String] = concat map (_ + "?")
  def zeroOrMore: Gen[String] = concat map (_ + "*")
  def oneOrMore: Gen[String] = concat map (_ + "+")
  def concat: Gen[String] = Gen listOf atoms map (_.mkString)
  def atoms: Gen[String] = Gen frequency (
    6 -> nonmeta,
    3 -> wildcard,
    1 -> subexpr
  )
  def nonmeta: Gen[String] = Gen resultOf ((_: String) filterNot metacharacters)
  def wildcard: Gen[String] = Gen value "."
  def subexpr: Gen[String] = regexp map ("(" + _ + ")")
}

class GenRegexSpecs extends Specification with ScalaCheck with DataTables {
  val generator = new GenRegex regexGenerator (_: String) apply;

  def is = 
    "Standalone Regular Expression Generator".title ^
    p                                                ^
    "A regular expression will generate a string matched by it" ! regexpMatchesGen ^
    p                                                ^
    "Examples"                                       ! patterns ^
    end

  def regexpMatchesGen = forAll(RegexGen.regexp) { regexp =>
    generator(regexp) matches regexp
  }

  def patterns =
    "Input"    || "Output"                   |
    ""         !! "<empty>"                  |
    "."        !! "a, b, c, ..."             |
    "a"        !! "a"                        |
    "b"        !! "b"                        |
    "aa"       !! "aa"                       |
    "ab"       !! "ab"                       |
    "ba"       !! "ba"                       |
    "abba"     !! "abba"                     |
    "a?"       !! "<empty>, a"               |
    "a|b"      !! "a, b"                     |
    "ab|ba"    !! "ab, ba"                   |
    "a|b|c"    !! "a, b, c"                  |
    "aa|bb|cc" !! "aa, bb, cc"               |
    "a(b|c)d"  !! "abd, acd"                 |> validate

  def validate = (input: String, output: String) => {
    val outputs = output split """\s*,\s*""" filterNot ("..." ==) map {
      case "<empty>" => ""
      case other     => other
    }
    def isComplete = !(output endsWith "...")

    def checkCompleteness =
      if (outputs.length == 1) generator(input) === outputs(0)
      else generator(input) must beOneOf(outputs: _*)

    def checkPresence =
      ((success: Result) /: outputs) { (result, expected) =>
        result and (generator(input) must beEqualTo(expected).eventually)
      }

    if (isComplete) checkCompleteness and checkPresence
    else checkPresence
  }
}
