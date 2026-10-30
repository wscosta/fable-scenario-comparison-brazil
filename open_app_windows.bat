@echo off
rem Generates only the map sets that are missing (instant when everything exists),
rem so a newly added scenario gets its maps without any manual step.
echo Checking maps...
Rscript "%~dp004_generate_maps.R" missing
if errorlevel 1 (
    echo Map generation failed. Opening app anyway.
    pause
)
Rscript "%~dp0app.R"
pause
