# Project:  CS 330
# Program:  Dynamic movement, 0 Control
# Purpose:  Control execution of dynamic movement algorithms.
# Author:   Mikel D. Petty, Ph.D., 256-824-6140, pettym@uah.edu
# Created:  2019-1-29
# Modified: 2022-2-9

rm(list = ls())      # Clear workspace
options(scipen=999)  # Suppress scientific notation

# Set execution control parameters.

computer <- 2        # Computer code is running on:  1=Alpha (OKT N353), 2=Bravo (27 CPD)
scenario <- 31       # Scenario to execute
warnings <- FALSE    # Display warning messages if unexpected/out of bounds values found?

# Initialize file paths and names.

if (computer == 1) {
  work.path     <- "C:/Users/mpetty/Desktop/Working, CS 330/"
  source.path   <- "C:/Users/mpetty/Desktop/Movement, Dynamic 22S/"
} else {
  work.path     <- "C:/Users/jpinckard/Desktop/Working, CS 330/"
  source.path   <- "C:/Users/jpinckard/Desktop/Movement, Dynamic 22S/"
}

trajectory.file <- paste(work.path, "CS 330, Dynamic ", scenario, ", Trajectory data.txt", sep="")

# Load support functions; initialize global and scenario variables.

source(paste(source.path, "CS 330, Dynamic movement, 1 Support v8.r", sep=""))
source(paste(source.path, "CS 330, Dynamic movement, 2 Initialize v12.r", sep=""))

# Execute movement to generate trajectories; plot trajectories.

source(paste(source.path, "CS 330, Dynamic movement, 3 Move v16.r", sep=""))
source(paste(source.path, "CS 330, Dynamic movement, 4 Plot v14.r", sep=""))

# End of program
