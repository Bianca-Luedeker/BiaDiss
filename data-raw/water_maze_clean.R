# This file cleans the water maze data and stores it in an Rda file.
# Read in the data.  Do some cleaning/verification
WaterMaze <- read.csv('data-raw/water_maze_data.csv')

# Save the data frame to the data/ directory as WaterMaze.rda
usethis::use_data(WaterMaze)
