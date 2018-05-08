package glue.data

trait AllFunctions
  extends OptionTFunctions
  with ReaderFunctions
  with StateFunctions
  with WriterFunctions

trait AllSyntax
  extends IdentitySyntax

trait AllImplicits
  extends ConstImplicits
  with IdentityImplicits
  with OptionTImplicits
  with ReaderImplicits
  with StateImplicits
  with WriterImplicits

object functions extends AllFunctions

object syntax extends AllSyntax

object implicits extends AllImplicits

object all extends AllFunctions with AllSyntax with AllImplicits
