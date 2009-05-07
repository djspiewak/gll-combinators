import org.specs._
import runner.JUnit4

object AllSpecs extends Specification {
  "GLL Combinators".isSpecifiedBy(
    ArithmeticSpecs,
    CompoundSpecs,
    DisjunctionSpecs,
    LineStreamSpecs,
    RegexSpecs,
    TerminalSpecs
  )
}

class AllSpecs extends JUnit4(AllSpecs)