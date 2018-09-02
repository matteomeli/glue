package glue
package data

trait AllFunctions
  extends IdTFunctions
  with EitherTFunctions
  with IndexedStateTFunctions
  with OptionTFunctions
  with ReaderFunctions
  with WriterTFunctions
  with WriterFunctions
  with StateFunctions

trait AllImplicits
  extends ConstImplicits
  with EitherTImplicits
  with IdentityImplicits
  with IdTImplicits
  with IndexedStateTImplicits
  with OptionTImplicits
  with ReaderImplicits
  with WriterTImplicits
  with WriterImplicits
  with StateImplicits

object functions extends AllFunctions

object implicits extends AllImplicits

object all extends AllFunctions with AllImplicits
