import org.specs._

object AllSpecs extends Specification {
  "GLL Combinators".isSpecifiedBy(
    ArithmeticSpecs,
    CompoundSpecs,
    DisjunctionSpecs,
    FilterSpecs,
    LineStreamSpecs,
    RegexSpecs,
    TerminalSpecs)
}
