package glue
package std

trait AllFunctions
  extends ListFunctions
  with OptionFunctions

trait AllSyntax
  extends IndexedSeqSyntax

trait AllImplicits
  extends AnyValImplicits
  with EitherImplicits
  with IndexedSeqImplicits
  with ListImplicits
  with OptionImplicits
  with StreamImplicits
  with StringImplicits

object functions extends AllFunctions

object syntax extends AllSyntax

object implicits extends AllImplicits

object all extends AllFunctions with AllSyntax with AllImplicits
