#!/bin/bash 
java -Xmx1024M -XX:PermSize=300m -jar `dirname $0`/sbt-launch.jar "$@"
