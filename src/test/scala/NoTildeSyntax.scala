import org.specs2.mutable._

trait NoTildeSyntax extends Specification {  
  override def stringToHtmlLinkFragments(str: String) = super.stringToHtmlLinkFragments(str)
  override def stringToHtmlLinkFragments2(str: String) = super.stringToHtmlLinkFragments2(str)
}
