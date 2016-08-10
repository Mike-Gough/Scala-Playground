import scala.util.{Failure, Success, Try}

package csc.membership {

  case class MshpNumber(number: String, prefix: String = "", pensionCode: String = "", suffix: String = "") extends MembershipNumber {
    var external_id = prefix + number + pensionCode + suffix

    override def validate: List[Try[String]] = {
      val (minLength, maxLength) = (4, 11)

      def validCase(id: String): Try[String] = {
        if (id.toUpperCase() != id)
          Failure(new IllegalArgumentException("must be in upper case."))
        else
          Success(id)
      }

      def validMinLength(id: String): Try[String] = {
        if (id.length <= minLength)
          Failure(new IllegalArgumentException(f"must be greater than or equal to $minLength%1d characters in length."))
        else
          Success(id)
      }

      def validMaxLength(id: String): Try[String] = {
        if (id.length > maxLength)
          Failure(new IllegalArgumentException(f"must be less than $maxLength%2d characters in length."))
        else
          Success(id)
      }

      def validSchemeCode(id: String): Try[String] = {
        val patternScheme = "[A-Z]{2}".r

        if (patternScheme.findFirstIn(id).isEmpty)
          Success(id)
        else
          List("CS", "PS", "OS", "PG", "MS", "DF", "DB", "AD").find(x => x == patternScheme.findFirstIn(id).getOrElse("not specified")) match {
            case Some(_) => Success(id)
            case None => Failure(new IllegalArgumentException("does not contain a valid pension code suffix. Valid codes include CS, PS, OS, PG, MS, DF, DB and AD."))
          }
      }

      def validFormat(id: String): Try[String] = {
        if (!id.matches("^((Z|P)?\\d{4,8}((CS|PS|OS|PG)|((CS|PS|OS|PG)[A-W]))?)|((A|N|R)?\\d{4,10}((MS|DF|DB|AD)|((MS|DF|DB|AD)[A-W])))(X|X\\d{2})?$"))
          Failure(new IllegalArgumentException("is not a valid Australian Government Service (AGS) or Military Service Number."))
        else
          Success(id)
      }

      // Return validation failures
      List(validCase(external_id), validMinLength(external_id), validMaxLength(external_id), validSchemeCode(external_id), validFormat(external_id)).filter(x => x.isFailure)
    }
  }

  object MshpNumber {
    // match Regex for format validation and decomposing
    val MembershipNumberPattern = "^(Z|P|A|N|R)?(\\d{4,10})(CS|PS|OS|PG|MS|DF|DB|AD)?([A-W]|X|X\\d{2})?$".r

    def unapply(id: MembershipNumber): Option[(String, String, String, String)] = {
      id.toString() match {
        case MembershipNumberPattern(prefix, number, pension, suffix) =>
          if (id.isValid)
            Some((prefix, number, pension, suffix))
          else
            None
        case _ => None
      }
    }
  }
}