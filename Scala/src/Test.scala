import csc.membership.MembershipNumber

object ScalaTest extends App {
  var printFormat: String = "%-16.16s %-8.8s %-150.150s%n"

  printf(printFormat, "#", "Valid?", "Validation Results")

  var member: MembershipNumber = MembershipNumber("a12345")
  printf(printFormat, member.Id, member.isValid(), member.validate()s)

  member = MembershipNumber("A12")
  printf(printFormat, member.Id, member.isValid(), member.validate())

  member = MembershipNumber("A123456789101112")
  printf(printFormat, member.Id, member.isValid(), member.validate())

  member = MembershipNumber("A12345CS")
  printf(printFormat, member.Id, member.isValid(), member.validate())

  member = MembershipNumber("A12345A")
  printf(printFormat, member.Id, member.isValid(), member.validate())

  member = MembershipNumber("A12345ADA")
  printf(printFormat, member.Id, member.isValid(), member.validate())
}