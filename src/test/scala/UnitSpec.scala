import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

abstract class UnitSpec extends AnyFlatSpec with GeneratorDrivenPropertyChecks with Matchers
