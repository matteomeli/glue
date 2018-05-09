package glue
package std

trait AllFunctions
  extends ListFunctions
  with OptionFunctions

trait AllImplicits
  extends AnyValImplicits
  with EitherImplicits
  with ListImplicits
  with OptionImplicits
  with StreamImplicits
  with StringImplicits

object functions extends AllFunctions

object implicits extends AllImplicits

object all extends AllFunctions with AllImplicits
