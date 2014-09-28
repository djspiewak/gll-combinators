import org.specs2.ScalaCheck
import org.specs2.mutable._

object ExamplesSpecs extends Specification with ScalaCheck {

  "Examples" should {
    "pass arithmetic test" in {
      arithmetic.ArithmeticParser.test("/arithmetic/error.txt") mustEqual false
      arithmetic.ArithmeticParser.test("/arithmetic/junk.txt") mustEqual true
      arithmetic.ArithmeticParser.test("/arithmetic/taylor-sin.txt") mustEqual true
    }

    "pass config test" in {
      config.ConfigParser.test("/config/input1.conf") mustEqual true
    }

    "pass lambdacalc test" in {
      lambdacalc.LambdaCalcParser.test("/lambdacalc/church.f") mustEqual true
      lambdacalc.LambdaCalcParser.test("/lambdacalc/fac.f") mustEqual true
    }

    "pass miniml test" in {
      miniml.MiniMLParser.test("/miniml/arity-clash.sml") mustEqual true
      miniml.MiniMLParser.test("/miniml/example1.sml") mustEqual true
      miniml.MiniMLParser.test("/miniml/example2.sml") mustEqual true
      miniml.MiniMLParser.test("/miniml/example4.sml") mustEqual true
    }

    "pass paren test" in {
      paren.ParenParser.test("/paren/error.txt") mustEqual false
      paren.ParenParser.test("/paren/input1.txt") mustEqual true
      paren.ParenParser.test("/paren/input2.txt") mustEqual true
    }
  }
}
