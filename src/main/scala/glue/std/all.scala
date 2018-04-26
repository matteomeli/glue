package glue.std

trait AllFunctions
  extends EitherFunctions
  with ListFunctions
  with OptionFunctions
  with StreamFunctions
  with StringFunctions

trait AllSyntax
  extends EitherSyntax
  with ListSyntax
  with OptionSyntax
  with StreamSyntax
  with StringSyntax

trait AllImplicits
  extends AnyValImplicits
  with EitherImplicits
  with ListImplicits
  with OptionImplicits
  with StreamImplicits
  with StringImplicits

object functions extends AllFunctions

object syntax extends AllSyntax

object implicits extends AllImplicits

object all extends AllFunctions with AllSyntax with AllImplicits
