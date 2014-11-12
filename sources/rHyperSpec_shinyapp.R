#rHyperSpec_shinyapp.R

#Written by Christine Laney, cmlaney@utep.edu
#Systems Ecology Lab, University of Texas at El Paso
#Last updated Oct 1, 2014
#Code repository with previous versions located at https://github.com/chrlaney

#Import required .csv files, which can be manipulated by the user

#list of spectral indices
indexlist <- read.csv("sources/Indices.csv", header = TRUE, strip.white = TRUE, 
  stringsAsFactors = FALSE)
#list of people involved with project
peoplelist <- read.csv("sources/people.csv", header = FALSE, stringsAsFactors = FALSE)
people <- as.character(peoplelist[,1])
#list of sampling paths (tramlines, transects, etc.)
samplingpathlist <- read.csv("sources/samplingpath.csv", header = FALSE, stringsAsFactors = FALSE)
samplingpath <- as.character(samplingpathlist[,1])
