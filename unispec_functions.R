#unispec_functions.r

#Get the event date as an integer (yyyymmdd) from the user; parse it correctly
#as data type numeric, character, or Date.
getDate <- function(type = "numeric"){
 varEntryDialog(vars=c('date'), 
                labels=c('Enter a date in integer form, e.g., yyyymmdd:'), 
                fun=c(function(x) {
                 if(nchar(x) != 8){print("Please enter a correct date")
                                   x = NA}
                 else{
                  x <- as.Date(x, format = "%Y%m%d")
                  if(type == "numeric"){
                   x <- as.numeric(x)
                  }
                  if(type == "Date"){
                   x <- as.Date(x, format = "%Y%m%d")
                  }
                  if(type == "character"){
                   x <- as.character(as.Date(x, format = "%Y%m%d"))
                  }
                  return(x)
                 }}))}

#The generic function for reading spu files - do not use.
spu <- function(x,...) UseMethod("spu")

#Function to parse the three data columns from an .spu file.
#Returns a data frame with three columns: wavelength, irradiance, and radiance
getdata.spu <- function(x,upchannel,...){
 spu <- read.delim(x, 
                   skip = 0, 
                   header = FALSE,
                   fill = TRUE,
                   col.names = c("wavelength","radiance","irradiance"),
                   stringsAsFactors = FALSE,
                   strip.white = TRUE
 )
 if(upchannel == "B"){names(spu) <- c("wavelength","irradiance","radiance")}
 data <- spu[-grep(pattern = '[[:alpha:]]', x = spu[,1]),]
 data[,1] <- as.numeric(data[,1])
 data
}

gettimestamp.spu <- function(x,...){
 spu <- read.delim(x, 
                   skip = 0, 
                   nrows = 3,
                   header = FALSE,
                   fill = TRUE,
                   stringsAsFactors = FALSE,
                   strip.white = TRUE)
 timestamp <- as.vector(spu[3,1])
 timestamp <- sub(timestamp, pattern = "Time:       ", replacement = "")
 timestamp <- as.POSIXct(timestamp, format = "%m/%d/%Y %I:%M:%S %p")
 return(timestamp)
}

#Function to parse the first nine lines of metadata from an .spu file.
getmetadata.spu <- function(x,...){
 spu <- read.delim(x, 
                   skip = 0, 
                   header = FALSE,
                   fill = TRUE,
                   stringsAsFactors = FALSE)
 metadata <- as.vector(spu[grep(pattern = '[[:alpha:]]', x = spu[,1]),])
 
 limits <- sub(x = metadata[4], pattern = "Limits:     ", replacement = "")
 limits2 <- strsplit(limits, " - ")
 env <- sub(x = metadata[5], pattern = "Environment:   ", replacement = "")
 env2 <- strsplit(env, " ")
 min <- sub(x = metadata[6], pattern = "Minimum:    ", replacement = "")
 min2 <- strsplit(min, "  ")
 max <- sub(x = metadata[7], pattern = "Maximum:    ", replacement = "")
 max2 <- strsplit(max, "  ")
 inttime <- gsub(metadata[8], pattern = "Integration:  ", replacement = "")
 inttime2 <- gsub(inttime, pattern = " ms", replacement = "")
 
 
 filename <- substr(metadata[1], nchar(metadata[1])-9, nchar(metadata[1]))
 equipment <- substr(metadata[2], nchar(metadata[2])-50, nchar(metadata[2])-31)
 netcf_ver <- substr(metadata[2], nchar(metadata[2])-15, nchar(metadata[2])-9)
 timestamp <- substr(metadata[3], nchar(metadata[3])-21, nchar(metadata[3]))
 wavelim_lower  <- as.numeric(limits2[[1]][1])
 wavelim_upper  <- as.numeric(limits2[[1]][2])
 degrees_c <- as.numeric(gsub(env2[[1]][1], pattern = "DegreesC=", replacement = ""))
 battery <- as.numeric(gsub(env2[[1]][2], pattern = "BattV=", replacement = ""))
 a1 <- as.numeric(gsub(env2[[1]][3], pattern = "A1=", replacement = ""))
 a2 <- as.numeric(gsub(env2[[1]][4], pattern = "A2=", replacement = ""))
 a3 <- as.numeric(gsub(env2[[1]][5], pattern = "A3=", replacement = ""))
 a4 <- as.numeric(gsub(env2[[1]][6], pattern = "A4=", replacement = ""))
 wave_min <- as.numeric(gsub(min2[[1]][1], pattern = "nm", replacement = ""))
 value_min <- as.numeric(min2[[1]][2])
 wave_max <- as.numeric(gsub(max2[[1]][1], pattern = "nm", replacement = ""))
 value_max <- as.numeric(max2[[1]][2])
 int_time <- as.numeric(inttime2)
 scans <- as.numeric(gsub(metadata[9], pattern = "Number Scans: ", replacement = ""))
 
 
 timestamp <- strptime(timestamp, format = "%m/%d/%Y %I:%M:%S %p")
 date_id <- as.numeric(format(timestamp, "%Y%m%d"))
 timestamp <- as.character(timestamp)
 
 m <- list(date_id = date_id, timestamp = timestamp, filename = filename, 
           equipment = equipment, netcf_ver = netcf_ver, wavelim_lower = wavelim_lower, 
           wavelim_upper = wavelim_upper, degrees_c = degrees_c, battery = battery, 
           a1 = a1, a2 = a2, a3 = a3, a4 = a4, wave_min = wave_min, 
           wave_max = wave_max, value_min = value_min, value_max = value_max, 
           int_time = int_time, scans = scans)
}


alt_getmetadata.spu <- function(x,...){
 spu <- read.delim(x, 
                   skip = 0, 
                   header = FALSE,
                   fill = TRUE,
                   stringsAsFactors = FALSE)
 metadata <- as.vector(spu[1:9,1])
 
 limits <- sub(x = metadata[4], pattern = "Limits:     ", replacement = "")
 limits2 <- strsplit(limits, " - ")
 env <- sub(x = metadata[5], pattern = "Environment:   ", replacement = "")
 env2 <- strsplit(env, " ")
 min <- sub(x = metadata[6], pattern = "Minimum:    ", replacement = "")
 min2 <- strsplit(min, "  ")
 max <- sub(x = metadata[7], pattern = "Maximum:    ", replacement = "")
 max2 <- strsplit(max, "  ")
 inttime <- gsub(metadata[8], pattern = "Integration:  ", replacement = "")
 inttime2 <- gsub(inttime, pattern = " ms", replacement = "")
 
 
 filename <- substr(metadata[1], nchar(metadata[1])-9, nchar(metadata[1]))
 equipment <- substr(metadata[2], nchar(metadata[2])-50, nchar(metadata[2])-31)
 netcf_ver <- substr(metadata[2], nchar(metadata[2])-15, nchar(metadata[2])-9)
 timestamp <- substr(metadata[3], nchar(metadata[3])-20, nchar(metadata[3]))
 wavelim_lower  <- as.numeric(limits2[[1]][1])
 wavelim_upper  <- as.numeric(limits2[[1]][2])
 degrees_c <- as.numeric(gsub(env2[[1]][1], pattern = "DegreesC=", replacement = ""))
 battery <- as.numeric(gsub(env2[[1]][2], pattern = "BattV=", replacement = ""))
 a1 <- as.numeric(gsub(env2[[1]][3], pattern = "A1=", replacement = ""))
 a2 <- as.numeric(gsub(env2[[1]][4], pattern = "A2=", replacement = ""))
 a3 <- as.numeric(gsub(env2[[1]][5], pattern = "A3=", replacement = ""))
 a4 <- as.numeric(gsub(env2[[1]][6], pattern = "A4=", replacement = ""))
 wave_min <- as.numeric(gsub(min2[[1]][1], pattern = "nm", replacement = ""))
 value_min <- as.numeric(min2[[1]][2])
 wave_max <- as.numeric(gsub(max2[[1]][1], pattern = "nm", replacement = ""))
 value_max <- as.numeric(max2[[1]][2])
 int_time <- as.numeric(inttime2)
 scans <- as.numeric(gsub(metadata[9], pattern = "Number Scans: ", replacement = ""))
 
 
 timestamp <- strptime(timestamp, format = "%m/%d/%Y %I:%M:%S %p")
 date_id <- as.numeric(format(timestamp, "%Y%m%d"))
 timestamp <- as.character(timestamp)
 
 m <- list(date_id = date_id, timestamp = timestamp, filename = filename, 
           equipment = equipment, netcf_ver = netcf_ver, wavelim_lower = wavelim_lower, 
           wavelim_upper = wavelim_upper, degrees_c = degrees_c, battery = battery, 
           a1 = a1, a2 = a2, a3 = a3, a4 = a4, wave_min = wave_min, 
           wave_max = wave_max, value_min = value_min, value_max = value_max, 
           int_time = int_time, scans = scans)
}


switchcols.spu <- function(x,...){
 x <- (x[,c(1,3,2)])
 names(x) <- c("wavelength","irradiance","radiance")
}

#The generic functions for the data frame generated from an .spu file
spudata <- function(x,...) UseMethod("spudata")

#Make sure that the irradiance column is second and the radiance column is third.
#Check the order of the columns in the data frame by comparing the values of the
#57th row - radiance should be higher than irradiance. If not, then the optic fibers
#were put into the incorrect ports, and the data columns need to be switched.
ordercols.spudata <- function(x,...){
 c <- x[57,3] > x[57,2]
 if(c == TRUE){
  cat("The radiance and irradiance columns in the .spu file are not ordered correctly\n")
  names(x) <- c("wavelength","radiance","irradiance")
  x <- (x[,c(1,3,2)]) 
  cat("The radiance and irradiance columns were switched and labelled correctly\n")
 } 
  return(x)
}

#summarize the spu data frame
summary.spudata <- function(x,...){
 x <- as.data.frame(x)
 rsummary <- summary(x)
 rsummary
}

#calculate the reflectance of the unispec data by dividing the irradiance by the
#radiance
refl.calc.spudata <- function(x,...){
 x <- as.data.frame(x)
 x$reflectance <- x$irradiance/x$radiance
 x
}

#Process white panel data - can use 1 or more files to calculate reflectance.
panelprocess.spudata <- function(files,filenames,upchannel,...){
  wavelengths <- getdata.spu(x = files[1], upchannel = upchannel)$wavelength
  paneldata <- data.frame(wavelength = wavelengths)
  
  #For each of the white panel files, read in the file, calculate reflectance, and 
  #add a column to the results file
  for(i in 1:length(files)){
   pfile <- getdata.spu(x = files[i], upchannel = upchannel)
   paneldata[,i+1] <- pfile$radiance/pfile$irradiance
   names(paneldata)[i+1] <- paste("refl_", filenames[i], sep = "")
  }
 
 #Calculate mean reflectance and the standard deviation of the reflectance at every 
 #wavelength measured in the raw data files
 if(length(files) == 1){
  paneldata$avg <- paneldata[,2]
  paneldata$sd <- rep(NA, nrow(paneldata))
 } else {
  paneldata$avg <- rowMeans(paneldata[,2:ncol(paneldata)])
  paneldata$avg[which(paneldata$avg > 1000)] <- NA
  paneldata$sd <- apply(paneldata[2:(ncol(paneldata)-1)],1,sd,na.rm=TRUE)
 }
 paneldata
}

#Create a user input dialog box using tcl/tk.
#Contributed by Jason Bryer, downloaded from http://bryer.org/2012/user-input-using-tcltk/
#See varEntryDialog.R for more information and testing function.
varEntryDialog <- function(vars, 
                           labels = vars,
                           fun = rep(list(as.character), length(vars)),
                           title = 'Variable Entry',
                           prompt = NULL) {
 require(tcltk)
 
 stopifnot(length(vars) == length(labels), length(labels) == length(fun))
 
 # Create a variable to keep track of the state of the dialog window:
 # done = 0; If the window is active
 # done = 1; If the window has been closed using the OK button
 # done = 2; If the window has been closed using the Cancel button or destroyed
 done <- tclVar(0)
 
 tt <- tktoplevel()
 tkwm.title(tt, title) 
 entries <- list()
 tclvars <- list()
 
 # Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, 
 # assign 2 to done.
 tkbind(tt,"<Destroy>",function() tclvalue(done)<-2)
 
 for(i in seq_along(vars)) {
  tclvars[[i]] <- tclVar("")
  entries[[i]] <- tkentry(tt, textvariable=tclvars[[i]])
 }
 
 doneVal <- as.integer(tclvalue(done))
 results <- list()
 
 reset <- function() {
  for(i in seq_along(entries)) {
   tclvalue(tclvars[[i]]) <<- ""
  }
 }
 reset.but <- tkbutton(tt, text="Reset", command=reset)
 
 cancel <- function() {
  tclvalue(done) <- 2
 }
 cancel.but <- tkbutton(tt, text='Cancel', command=cancel)
 
 submit <- function() {
  for(i in seq_along(vars)) {
   tryCatch( {
    results[[vars[[i]]]] <<- fun[[i]](tclvalue(tclvars[[i]]))
    tclvalue(done) <- 1
   },
             error = function(e) { tkmessageBox(message=geterrmessage()) },
             finally = { }
   )
  }
 }
 submit.but <- tkbutton(tt, text="Submit", command=submit)
 
 if(!is.null(prompt)) {
  tkgrid(tklabel(tt,text=prompt), columnspan=3, pady=10)
 }
 
 for(i in seq_along(vars)) {
  tkgrid(tklabel(tt, text=labels[i]), entries[[i]], pady=10, padx=10, columnspan=4)
 }
 
 tkgrid(submit.but, cancel.but, reset.but, pady=10, padx=10, columnspan=3)
 tkfocus(tt)
 
 # Do not proceed with the following code until the variable done is non-zero.
 #   (But other processes can still run, i.e. the system is not frozen.)
 tkwait.variable(done)
 
 if(tclvalue(done) != 1) {
  results <- NULL
 }
 
 tkdestroy(tt)
 return(results)
}

#Plot radiance vs. irradiance for error checking purposes
radVsIrrad.plot <- function(x,irr.col= "red", rad.col = "blue",...){
 x <- as.data.frame(x)
 plot(x[,2] ~ x[,1],
      ylim = c(0,60000),
      xlab = names(x)[1], 
      ylab = "value",
      type = "p",
      col = irr.col)
 lines(x[,2] ~ x[,1],
       col= irr.col)
 points(x[,3] ~ x[,1],
        col = rad.col)
 lines(x[,3] ~ x[,1],
       col= rad.col)
 legend(x = 900, y = 59000, c("Irradiance","Radiance"), col = c(irr.col, rad.col), 
        cex = 1, lwd = 1, x.intersp = 2, text.width = 100)
}


#Plot the white panel reflectances (reflectance over wavelength) 
panelReflectance.plot <- function(data, date_str, ...){
 colors <- rainbow(ncol(data))
 par(mfrow = c(1,2), oma = c(2,2,2,2), mar = c(5,5,2,2))
 plot(data$wavelength, data[,2], type = "l", col = colors[1], cex = .5,  
      ylim = c(0,5), xlab = "Wavelength (nm)", ylab = "White Panel Reflectance",
      main = paste("White Panel Reflectance Data ", date_str, sep = "")
 )
 
 for(i in 2:ncol(data)-1){
  lines(data$wavelength, data[,i], col = colors[i-1], cex = .5)
 }
 
 
 abline(h = seq(0,5,by = 0.5), lwd = 0.5, col = "gray")
 abline(v = seq(300,1150,50), lwd = 0.5, col = "gray")
 
 plot(x = 1:5, y = 1:5, xaxt = "n", yaxt = "n", type = "n", bty = "n",
      xlab = "", ylab = "", main = "")
 legend(x = 1, y = 5, names(data[1:ncol(data)-1]), col = colors,bty = "n",
        cex = 1, lwd = 0.5, x.intersp = 1, text.width = 4, ncol = 2)
 
}

#Format multiple data files into a single data frame with wavelength, irradiance,
#radiance, timestamp, and file id.
formatdata <- function(x){
 data <- getdata.spu(x)
 timestamp <- gettimestamp.spu(x)
 data$timestamp <- rep(as.POSIXct(timestamp, tz = "America/Dawson_Creek"), nrow(data))
 data$source_id <- rep(as.character(x), nrow(data))
 return(data)
}

calavg <- function(x, colorder){
 caldata <- getdata.spu(x)
 if(colorder == FALSE){data <- switchcols.spu(data)}
 return(data)
}



#For each file in a run, read the data, calculate reflectance, and then calculate 
#indices such as NDVI, WBI, PRI, etc.
calc_normalrefl <- function(calrefl, eventfile, interpolation, minwave, maxwave, upchannel) {
  data <- getdata.spu(eventfile, upchannel)
  allwaves <- seq(minwave, maxwave, 1) 
  #Linear
  if(interpolation == "linear"){
   InterCalIrr <- approx(x = data$wavelength, y = data$irradiance,
                         xout = allwaves,  method = "linear", yleft = NA, yright = NA, rule = 1, 
                         f = 0, ties = mean)
   InterCalRad <- approx(x = data$wavelength, y = data$radiance,
                         xout = allwaves,  method = "linear", yleft = NA, yright = NA, rule = 1, 
                         f = 0, ties = mean)
   InterWhite <- approx(x = data$wavelength, y = calrefl$avg,
                        xout = allwaves,  method = "linear", yleft = NA,
                        yright = NA, rule = 1, f = 0, ties = mean)
  }
  
  #Spline 
  if (interpolation == "spline"){
   InterCalIrr <- spline(x = data$wavelength, y = data$irradiance,
                         xout = allwaves, method = "natural", ties = mean)
   InterCalRad <- spline(x = data$wavelength, y = data$radiance,
                         xout = allwaves, method = "natural", ties = mean)
   InterWhite <- spline(x = data$wavelength, y = calrefl$avg,
                        xout = allwaves, method = "natural", ties = mean)  
  }
  
  #Cubic Interpolation using the Forsythe, Malcolm and Moler method as described
  #in the R help files for the spline() function.
  #"...an exact cubic is fitted through the four points at each end of the data, 
  #and this is used to determine the end conditions"
  if (interpolation == "cubic"){
   InterCalIrr <- spline(x = data$wavelength, y = data$irradiance,
                         xout = allwaves, method = "fmm", ties = mean)
   InterCalRad <- spline(x = data$wavelength, y = data$radiance,
                         xout = allwaves, method = "fmm", ties = mean)
   InterWhite <- spline(x = data$wavelength, y = calrefl$avg,
                        xout = allwaves, method = "fmm", ties = mean)  
  }
  
  interpdata <- data.frame(wavelength = InterCalIrr$x, irradiance = InterCalIrr$y,
                           radiance = InterCalRad$y, calrefl = InterWhite$y)
  
  #Step 2: Calculate reflectance data by dividing the radiance by the irradiance
  #at each wavelength
  interpdata$refl <- (interpdata$radiance/interpdata$irradiance)
  
  #Step 3: Calculate the final reflectance by comparing the irradiance signal to
  #the mean white panel signal (that is, normalize the data)
  interpdata$normrefl <- (interpdata$refl/interpdata$calrefl)
  
  #Step 4: Remove values outside of the possible range of -1 to 1
   interpdata$normrefl[which(interpdata$normrefl > 1)] <- NA
   interpdata$normrefl[which(interpdata$normrefl < -1)] <- NA

  return(interpdata)
}

calc_ndvi <- function(interpdata){
 p680 <- interpdata$normrefl[which(interpdata$wavelength == 680)]
 p800 <- interpdata$normrefl[which(interpdata$wavelength == 800)]
 ndvi <- (p800 - p680)/(p800 + p680)
 return(ndvi)
}

#Calculate the photochemical reflectance index, as defined by John Gamon et al 1992
calc_pri_gamon1992 <- function(interpdata){
 p531 <- interpdata$normrefl[which(interpdata$wavelength == 531)]
 p570 <- interpdata$normrefl[which(interpdata$wavelength == 570)]
 pri <- (p531 - p570)/(p531 + p570)
 return(pri)
}

#Calculate the photochemical reflectance index, as defined by John Gamon et al 1992
calc_pri_gamon <- function(interpdata){
 p531 <- interpdata$normrefl[which(interpdata$wavelength == 531)]
 p570 <- interpdata$normrefl[which(interpdata$wavelength == 570)]
 pri <- (p570 - p531)/(p570 + p531)
 return(pri)
}

#Calculate the water band index, as defined by Claudio et al 2006
calc_wbi_claudio <- function(interpdata){
 p900 <- interpdata$normrefl[which(interpdata$wavelength == 900)]
 p970 <- interpdata$normrefl[which(interpdata$wavelength == 970)]
 wbi <- (p900/p970) 
 return(wbi)
}
