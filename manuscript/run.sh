# 1. Install giExtract from PyPi
pip install giExtract

# 2. Get cubes from H&E images
giCube -p ~nat_com/CNESCC_JPF_sf8

# 3. Extract features
giExtract -p ~nat_com/CNESCC_JPF_sf8/cubes -c ~nat_com/CNESCC_JPF_sf8/context.csv

# 4. Perform differential feature analysis similar to mansucript
script=github_clone/giExtract/R/giFeature.R
Rscript $script ~nat_com/CNESCC_JPF_sf8/context.csv ~nat_com/CNESCC_JPF_sf8/features.csv "Name" "slide" "Group"

# 5. Manusript plots
# Load the files from step 4 into R and run the scripts under downstream.R


