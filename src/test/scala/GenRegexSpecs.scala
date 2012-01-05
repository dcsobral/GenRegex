import org.specs2.mutable._

class GenRegexSpecs extends Specification {
  "The '' regex" should {
    "generate an empty string" in {
      (new GenRegex regexGenerator "")() === ""
    }
  }
  "The 'a' regex" should {
    "generate a" in {
      (new GenRegex regexGenerator "a")() === "a"
    }
  }
  "The 'b' regex" should {
    "generate b" in {
      (new GenRegex regexGenerator "b")() === "b"
    }
  }
  "The 'ab' regex" should {
    "generate ab" in {
      (new GenRegex regexGenerator "ab")() === "ab"
    }
  }
  "The 'a|b' regex" should {
    "generate a or b" in {
      (new GenRegex regexGenerator "a|b")() must beOneOf("a", "b")
    }
    "generate a eventually" in {
      (new GenRegex regexGenerator "a|b")() must beEqualTo("a").eventually
    }
    "generate b eventually" in {
      (new GenRegex regexGenerator "a|b")() must beEqualTo("b").eventually
    }
  }
  "The 'ab|ba' regex" should {
    "generate ab or ba" in {
      (new GenRegex regexGenerator "ab|ba")() must beOneOf("ab", "ba")
    }
    "generate ab eventually" in {
      (new GenRegex regexGenerator "ab|ba")() must beEqualTo("ab").eventually
    }
    "generate ba eventually" in {
      (new GenRegex regexGenerator "ab|ba")() must beEqualTo("ba").eventually
    }
  }
}
