package fpinscala

import fpinscala.datastructures.{ListProps, TreeProps}
import fpinscala.errorhandling.{EitherProps, OptionProps}
import fpinscala.gettingstarted.GettingStartedProps

@main def checkAll(): Unit =
  GettingStartedProps.checkGettingStarted()
  ListProps.checkList()
  TreeProps.checkTree()
  OptionProps.checkOption()
  EitherProps.checkEither()
