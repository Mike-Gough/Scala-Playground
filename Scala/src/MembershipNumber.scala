import scala.util.{Failure, Success, Try}

package csc.membership {

  case class MembershipNumber(id: String) {
    var Id: String = id

    override def toString: String = Id.toUpperCase()

    def validate: List[Try[String]] = {
      val IdLengthMinimum: Int = 4
      val IdLengthMaximum: Int = 11
      val schemes = List("CS", "PS", "OS", "PG", "MS", "DF", "DB", "AD")
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
        val result = patternScheme.findFirstIn(id)

        if (result.isEmpty)
          Success(id)
        else
          schemes.find(x => x == result.getOrElse("not specified")) match {
            case Some(_) => Success(id)
            case None => Failure(new IllegalArgumentException("does not contain a valid pension code suffix. Valid codes include CS, PS, OS, PG, MS, DF, DB and AD."))
        }
      }

      def validFormat(id: String): Try[String] = {
        if (!id.matches(patternFormat.regex))
          Failure(new IllegalArgumentException("is not a valid Australian Government Service (AGS) or Military Service Number."))
        else
          Success(id)
      }

      // Return validation failures
      List(validCase(Id), validMinLength(Id), validMaxLength(Id), validSchemeCode(Id), validFormat(Id)).filter(x => x.isFailure)
    }

    def isValid: Boolean = {
      validate.forall(x => x.isSuccess)
    }
  }
}