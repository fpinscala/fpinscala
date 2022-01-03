package fpinscala

import fpinscala.datastructures.{ListProps, TreeProps}
import fpinscala.gettingstarted.GettingStartedProps

@main def checkAll(): Unit =
  GettingStartedProps.checkGettingStarted()
  ListProps.checkList()
  TreeProps.checkTree()
