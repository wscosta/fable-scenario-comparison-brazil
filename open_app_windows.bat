@echo off
if not exist "%~dp0data\maps\ct\landcover\landcover_Forest_2020.png" (
    echo Maps not found. Generating maps...
    Rscript "%~dp004_generate_maps.R"
    if errorlevel 1 (
        echo Map generation failed. Opening app anyway.
        pause
    ) else (
        echo Maps generated successfully.
    )
)
Rscript "%~dp0app.R"
pause
