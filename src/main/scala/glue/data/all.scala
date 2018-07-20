package glue
package data

trait AllFunctions
  extends IdTFunctions
  with EitherTFunctions
  with OptionTFunctions
  with KleisliFunctions
  with WriterTFunctions
  with IndexedStateTFunctions

trait AllImplicits
  extends ConstImplicits
  with EitherTImplicits
  with IdentityImplicits
  with IdTImplicits
  with OptionTImplicits
  with KleisliImplicits
  with WriterTImplicits
  with IndexedStateTImplicits

object functions extends AllFunctions

object implicits extends AllImplicits

object all extends AllFunctions with AllImplicits
