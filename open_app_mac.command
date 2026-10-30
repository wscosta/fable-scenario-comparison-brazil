#!/bin/bash
cd "$(dirname "$0")"
# Generates only the map sets that are missing (instant when everything exists),
# so a newly added scenario gets its maps without any manual step.
echo "Checking maps..."
Rscript 04_generate_maps.R missing
if [ $? -ne 0 ]; then
    echo "Map generation failed. Opening app anyway."
    read -r -p "Press Enter to continue..."
fi
Rscript app.R
