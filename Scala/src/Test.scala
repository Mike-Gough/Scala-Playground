import csc.membership.{MembershipNumber, MshpNumber}
import scala.util.{Success,Failure}

object ScalaTest extends App {
  var printFormat: String = "%-16.16s %-8.8s %-150.150s%n"

  printf(printFormat, "#", "Valid?", "Validation Results")

  var mn: MembershipNumber = new MshpNumber("a12345")
  printf(printFormat, mn.number, mn.isValid, mn.isValid match {
    case true => ""
    case false => mn.validate.head
  })

  mn = new MshpNumber("A12")
  printf(printFormat, mn.number, mn.isValid, mn.isValid match {
    case true => ""
    case false => mn.validate.head
  })

  mn = new MshpNumber("A123456789101112")
  printf(printFormat, mn.number, mn.isValid, mn.isValid match {
    case true => ""
    case false => mn.validate.head
  })

  mn = new MshpNumber("A12345CS")
  printf(printFormat, mn.number, mn.isValid, mn.isValid match {
    case true => ""
    case false => mn.validate.head
  })

  mn = new MshpNumber("A12345LA")
  printf(printFormat, mn.number, mn.isValid, mn.isValid match {
    case true => ""
    case false => mn.validate.head
  })

  mn = new MshpNumber("A12345A")
  printf(printFormat, mn.number, mn.isValid, mn.isValid match {
    case true => ""
    case false => mn.validate.head
  })

  mn = new MshpNumber("A12345ADA")
  printf(printFormat, mn.number, mn.isValid, mn.isValid match {
    case true => ""
    case false => mn.validate.head
  })

  var printFormat1: String = "%-12.12s %-12.12s %-12.12s %-12.12s%n"
  println
  printf(printFormat1, "Prefix", "#", "Scheme", "Suffix")

  // given a membership number, extract the components we need to match on
  mn match {
    case MshpNumber("A", number, "AD", suffix) =>
      printf(printFormat1, "Army", number, "ADFS", suffix)
    case _ =>
      printf("Invalid membership number")
  }
}