@echo off
cls

if exist bin goto :end_mkdir
    mkdir bin
:end_mkdir

cd bin
@echo on
call scalac -deprecation -unchecked -sourcepath ../src ../src/amid/bio/Main.scala
echo fsc result: %errorlevel%
echo -------------

IF ERRORLEVEL 1 GOTO :END
rem call scala amid.bio.Run

:END

cd ..