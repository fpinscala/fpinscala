package fpinscala.tests

import fpinscala.tests.datastructures.{ListProps, TreeProps}
import fpinscala.tests.errorhandling.{EitherProps, OptionProps}
import fpinscala.tests.gettingstarted.GettingStartedProps

@main def checkAll(): Unit =
  GettingStartedProps.checkGettingStarted()
  ListProps.checkList()
  TreeProps.checkTree()
  OptionProps.checkOption()
  EitherProps.checkEither()
