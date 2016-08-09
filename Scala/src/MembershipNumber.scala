import scala.util.{Failure, Success, Try}

package csc.membership {

  // Member class
  case class MembershipNumber(accountNumber: String) {
    var Id: String = accountNumber

    override def toString: String = Id.toUpperCase()

    // Constants
    var IdLengthMinimum: Int = 4
    var IdLengthMaximum: Int = 11

    // Validate function
    def validate(): List[Try[String]] = {
      val patternFormat = "^((Z|P)?\\d{4,8}((CS|PS|OS|PG)|((CS|PS|OS|PG)[A-W]))?)|((A|N|R)?\\d{4,10}((MS|DF|DB|AD)|((MS|DF|DB|AD)[A-W])))(X|X\\d{2})?$".r
      val patternScheme = "[A-Z]{2}".r

      def validCase(id: String): Try[String] = {
        if (id.toUpperCase() != id)
          Failure(new IllegalArgumentException("must be in upper case."))
        else
          Success(id)
      }

      def validMinLength(id: String): Try[String] = {
        if (id.length <= IdLengthMinimum)
          Failure(new IllegalArgumentException(f"must be greater than or equal to $IdLengthMinimum%1d characters in length."))
        else
          Success(id)
      }

      def validMaxLength(id: String): Try[String] = {
        if (id.length > IdLengthMaximum)
          Failure(new IllegalArgumentException(f"must be less than $IdLengthMaximum%2d characters in length."))
        else
          Success(id)
      }

      def validSchemeCode(id: String): Try[String] = {
        val schemes = List("CS", "PS", "OS", "PG", "MS", "DF", "DB", "AD")
        val result = patternScheme.findFirstIn(id)

        if (result.isEmpty)
          Success(id)
        else {
          schemes.find(x => x == result.getOrElse("not specified")) match {
            case Some(_) => Success(id)
            case None => Failure(new IllegalArgumentException("does not contain a valid pension code suffix. Valid codes include CS, PS, OS, PG, MS, DF, DB and AD."))
          }
        }
      }

      def validFormat(id: String): Try[String] = {
        if (!id.matches(patternFormat.regex))
          Failure(new IllegalArgumentException("is not a valid Australian Government Service (AGS) or Military Service Number."))
        else
          Success(id)
      }

      // Return validation failure descriptions
      val results: List[Try[String]] = List(validCase(Id), validMinLength(Id), validMaxLength(Id), validSchemeCode(Id), validFormat(Id)).filter(x => x.isFailure)
      return results
    }

    def isValid(): Boolean = {
      return validate().forall(x => x.isSuccess)
    }
  }
}