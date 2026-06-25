#!/bin/bash
cd "$(dirname "$0")"
if [ ! -f "data/maps/diff/landcover/landcover_Forest_2020.png" ]; then
    echo "Maps not found. Generating maps..."
    Rscript 04_generate_maps.R
    if [ $? -ne 0 ]; then
        echo "Map generation failed. Opening app anyway."
        read -r -p "Press Enter to continue..."
    else
        echo "Maps generated successfully."
    fi
fi
Rscript app.R
