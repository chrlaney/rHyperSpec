#rHyperSpec_shinyapp_withDB.R

#Written by Christine Laney, cmlaney@utep.edu
#Systems Ecology Lab, University of Texas at El Paso
#Last updated Feb 17, 2014
#Code repository with previous versions located at https://github.com/chrlaney

#Set working directory
setwd("/Users/cmlaney/Documents/Projects/rHyperSpec")
indexlist <- read.csv("Indices.csv", header = TRUE, strip.white = TRUE, 
  stringsAsFactors = FALSE)
dir.create("/Users/cmlaney/Documents/Projects/rHyperSpec/outputFiles/plots")
dir.create("/Users/cmlaney/Documents/Projects/rHyperSpec/outputFiles/reports")
dir.create("/Users/cmlaney/Documents/Projects/rHyperSpec/outputFiles/tables")

#make sure all of these libraries are installed, and then load them.
require(shiny)
require(ggplot2)
require(reshape2)
require(scales)
require(grid) #not available for 3.0.2
require(MASS) #for RML method of smoothing
require(mgcv) #for GAM method of smoothing
require(RCurl) #check internet connection for googleVis
require(knitr)

runApp()
