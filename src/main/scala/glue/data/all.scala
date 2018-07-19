package glue
package data

trait AllFunctions
  extends IdTFunctions
  with EitherTFunctions
  with OptionTFunctions
  with ReaderFunctions
  with StateFunctions
  with WriterTFunctions
  with WriterFunctions
  with IndexedStateTFunctions

trait AllImplicits
  extends ConstImplicits
  with EitherTImplicits
  with IdentityImplicits
  with IdTImplicits
  with OptionTImplicits
  with ReaderImplicits
  with StateImplicits
  with WriterTImplicits
  with WriterImplicits
  with IndexedStateTImplicits

object functions extends AllFunctions

object implicits extends AllImplicits

object all extends AllFunctions with AllImplicits
