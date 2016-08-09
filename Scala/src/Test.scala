import csc.membership.MembershipNumber

object ScalaTest extends App {
  var printFormat: String = "%-16.16s %-8.8s %-150.150s%n"

  printf(printFormat, "#", "Valid?", "Validation Results")

  var mn: MembershipNumber = MembershipNumber("a12345")
  printf(printFormat, mn.Id, mn.isValid, mn.validate)

  mn = MembershipNumber("A12")
  printf(printFormat, mn.Id, mn.isValid, mn.validate)

  mn = MembershipNumber("A123456789101112")
  printf(printFormat, mn.Id, mn.isValid, mn.validate)

  mn = MembershipNumber("A12345CS")
  printf(printFormat, mn.Id, mn.isValid, mn.validate)

  mn = MembershipNumber("A12345A")
  printf(printFormat, mn.Id, mn.isValid, mn.validate)

  mn = MembershipNumber("A12345ADA")
  printf(printFormat, mn.Id, mn.isValid, mn.validate)
}