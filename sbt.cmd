@echo off
set SCRIPT_DIR=%~dp0
java %SBT_OPTS% -Xmx512m -Xss8M -jar "%SCRIPT_DIR%sbt-launch.jar" %*
