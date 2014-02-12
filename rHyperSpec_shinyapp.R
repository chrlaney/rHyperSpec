#rHyperSpec_shinyapp_withDB.R

#Written for by Christine Laney, cmlaney@utep.edu
#Systems Ecology Lab, University of Texas at El Paso
#Last updated Jan. 16, 2014
#Code repository with previous versions located at https://github.com/chrlaney

#Set working directory
setwd("/Users/cmlaney/Documents/GitHub/rHyperSpec")
indexlist <- read.csv("Indices.csv", header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)

#make sure all of these libraries are installed, and then load them.
require(shiny)
require(ggplot2)
require(reshape2)
require(scales)
require(grid) #not available for 3.0.2
require(MASS) #for RML method of smoothing
require(mgcv) #for GAM method of smoothing
require(googleVis) #for pretty tables
require(RCurl) #check internet connection for googleVis

online <- NA
onlinecheck <- NA
onlinecheck <- getURL("www.google.com")
if(!is.na(onlinecheck)){online <- TRUE} else {online <- FALSE}

runApp()
