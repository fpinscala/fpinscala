@echo off
set SCRIPT_DIR=%~dp0
set ANT_OPTS=-Dsbt.ivy.home=C:/Users/%USERNAME%/.ivy2/

java %SBT_OPTS% -Xmx1024m -Dsbt.ivy.home=C:/Users/%USERNAME%/.ivy2/ -Dfile.encoding=UTF-8 -Xss40M -XX:MaxPermSize=256M -XX:NewSize=128M -XX:NewRatio=3 -jar "%SCRIPT_DIR%sbt-launch.jar" %*
