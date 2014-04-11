#rHyperSpec_shinyapp_withDB.R

#Written by Christine Laney, cmlaney@utep.edu
#Systems Ecology Lab, University of Texas at El Paso
#Last updated April 11, 2014
#Code repository with previous versions located at https://github.com/chrlaney

#Set working directory
setwd("/Users/cmlaney/Documents/Projects/rHyperSpec")

#Required .csv files, which can be manipulated by the user
#list of spectral indices
indexlist <- read.csv("Indices.csv", header = TRUE, strip.white = TRUE, 
  stringsAsFactors = FALSE)
#list of people involved with project
peoplelist <- read.csv("people.csv", header = FALSE, stringsAsFactors = FALSE)
people <- as.character(peoplelist[,1])
#list of sampling paths (tramlines, transects, etc.)
samplingpathlist <- read.csv("samplingpath.csv", header = FALSE, stringsAsFactors = FALSE)
samplingpath <- as.character(samplingpathlist[,1])

#create directories for output files. Be sure to copy these folders to another place 
#with the correct date and other info before running the program on another batch of files.
dir.create("/Users/cmlaney/Documents/Projects/rHyperSpec/outputFiles")
dir.create("/Users/cmlaney/Documents/Projects/rHyperSpec/outputFiles/plots")
dir.create("/Users/cmlaney/Documents/Projects/rHyperSpec/outputFiles/reports")
dir.create("/Users/cmlaney/Documents/Projects/rHyperSpec/outputFiles/tables")

#install and load required libraries
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
