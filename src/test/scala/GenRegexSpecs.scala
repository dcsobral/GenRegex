import org.specs2._
import specification.gen._
import matcher.DataTables
import execute.Result
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink

object RegexGen {
  val metacharacters = ".|?*+{}[]()".toSet
  
  def regexp: Gen[String] = alt(3)
  def alt(implicit maxDepth: Int): Gen[String] = Gen listOf concat map (_ mkString "|")
  def concat(implicit maxDepth: Int): Gen[String] = Gen listOf reps map (_.mkString)
  def reps(implicit maxDepth: Int): Gen[String] = Gen oneOf (zeroOrOne, zeroOrMore, oneOrMore)
  def zeroOrOne(implicit maxDepth: Int): Gen[String] = atoms map (_ + "?")
  def zeroOrMore(implicit maxDepth: Int): Gen[String] = atoms map (_ + "*")
  def oneOrMore(implicit maxDepth: Int): Gen[String] = atoms map (_ + "+")
  def atoms(implicit maxDepth: Int): Gen[String] = Gen frequency (
    6 -> nonmeta,
    3 -> wildcard,
    1 -> subexpr
  )
  def nonmeta(implicit maxDepth: Int) = Gen.alphaStr map (s => if (s.isEmpty) "x" else s)
    //Gen resultOf { (s: String) =>
      //s filter (c => c.isValidChar && !c.isControl && !metacharacters(c))
    //}
  def wildcard(implicit maxDepth: Int) = Gen value "."
  def subexpr(implicit maxDepth: Int): Gen[String] =
    if (maxDepth > 0) alt(maxDepth - 1) map ("(" + _ + ")")
    else ""
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

  def regexpMatchesGen = forAllNoShrink(RegexGen.regexp) { regexp =>
    val sample = generator(regexp)
    (sample matches regexp) :| 
    """Sample "%s" isn't matched by regexp /%s/""".format(sample, regexp)
  }

  def patterns =
    "Input"    || "Output"                   |
    ""         !! "<empty>"                  |
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
    val outputs = output split """\s*,\s*""" map {
      case "<empty>" => ""
      case other     => other
    }
    def noUnexpectedResult =
      if (outputs.length == 1) generator(input) === outputs(0)
      else generator(input) must beOneOf(outputs: _*)

    def allExpectedResults =
      ((success: Result) /: outputs) { (result, expected) =>
        result and (generator(input) must beEqualTo(expected).eventually)
      }

    noUnexpectedResult and allExpectedResults
  }
}
