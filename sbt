#!/bin/bash 
java -Xmx1024M -Xss8m -XX:PermSize=300m -jar `dirname $0`/sbt-launch.jar "$@"
