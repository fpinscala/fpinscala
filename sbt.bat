set SCRIPT_DIR=%~dp0
java -Xmx1024M -XX:PermSize=300m -jar "%SCRIPT_DIR%sbt-launch.jar" %*

