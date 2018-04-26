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

trait AllInstances
  extends AnyValInstances
  with EitherInstances
  with ListInstances
  with OptionInstances
  with StreamInstances
  with StringInstances

object functions extends AllFunctions

object syntax extends AllSyntax

object instances extends AllInstances

object all extends AllFunctions with AllSyntax with AllInstances
