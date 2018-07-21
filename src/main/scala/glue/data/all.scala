package glue
package data

trait AllFunctions
  extends IdTFunctions
  with EitherTFunctions
  with IndexedStateTFunctions
  with OptionTFunctions
  with ReaderFunctions
  with StateFunctions
  with WriterTFunctions
  with WriterFunctions

trait AllImplicits
  extends ConstImplicits
  with EitherTImplicits
  with IdentityImplicits
  with IdTImplicits
  with IndexedStateTImplicits
  with OptionTImplicits
  with ReaderImplicits
  with StateImplicits
  with WriterTImplicits
  with WriterImplicits

object functions extends AllFunctions

object implicits extends AllImplicits

object all extends AllFunctions with AllImplicits
