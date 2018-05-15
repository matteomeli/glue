package glue
package data

trait AllFunctions
  extends IdTFunctions
  with EitherTFunctions
  with OptionTFunctions
  with ReaderFunctions
  with StateFunctions
  with WriterFunctions

trait AllSyntax
  extends IdentitySyntax

trait AllImplicits
  extends ConstImplicits
  with EitherTImplicits
  with IdentityImplicits
  with IdTImplicits
  with OptionTImplicits
  with ReaderImplicits
  with StateImplicits
  with WriterImplicits

object functions extends AllFunctions

object syntax extends AllSyntax

object implicits extends AllImplicits

object all extends AllFunctions with AllSyntax with AllImplicits
