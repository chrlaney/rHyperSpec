#server.R
#For rHyperSpec_shinyapp

shinyServer(function(input, output) {
 
 #### Reactive Functions ####
 
 getCalTimestamp <- reactive({
  #Read the first white panel calibration file, extract the timestamp 
  data <- read.table(as.character(input$calfiles[1,4]),skip = 2, nrow = 1,
                    fill = FALSE, header = FALSE, 
                    stringsAsFactors = FALSE, strip.white = TRUE)
  timestamp <- sub(as.vector(data[1,1]), pattern = "Time:       ", replacement = "")
  timestamp <- as.POSIXct(timestamp, format = "%m/%d/%Y %I:%M:%S %p")
  return(timestamp)
 })
 
 getEventTimestamp <- reactive({
  #Read the first white panel calibration file, extract the timestamp 
  data <- read.table(as.character(input$eventfiles[1,4]),skip = 2, nrow = 1,
                     fill = FALSE, header = FALSE, 
                     stringsAsFactors = FALSE, strip.white = TRUE)
  timestamp <- sub(as.vector(data[1,1]), pattern = "Time:       ", replacement = "")
  timestamp <- as.POSIXct(timestamp, format = "%m/%d/%Y %I:%M:%S %p")
  return(timestamp)
 })
 

 getDate <- reactive({
  date <- substr(as.character(getCalTimestamp()), 1, 10)
  return(date)
 })
 
 getDataFromFile <- function(filepath, chan){
  #Extract the data from an .spu file. The resulting data frame has three
  #columns: wavelength, radiance, and irradiance. If the user checks that the
  #up fiber was attached to channel B, then switch the column names for 
  #radiance and irradiance.
  data <- read.delim(filepath, fill = TRUE,
                     col.names = c("wavelength","radiance","irradiance"),
                     stringsAsFactors = FALSE, strip.white = TRUE)
  data <- data[-grep(pattern = '[[:alpha:]]', x = data[,1]),]
  data[,1] <- as.numeric(data[,1])
  if(chan == "B"){names(data) <- c("wavelength","irradiance","radiance")}
  return(data)
 }
 
 getInterpNormRefl <- function(eventdata, calrelfavgs, interpolation){
  
   
  #Interpolate the radiance and irradiance data for a single event file.
  #Then calculate normalized reflectance for the event file.
  #Arguments include a single event dataset, the average panel reflectance, and
  #the interpolation type (linear, spline, or cubic).
  #Return a data frame with four columns: wavelength, irradiance, radiance, and 
  #normrefl (normal reflectance)
  data <- eventdata
  calrefl <- calrelfavgs
  interpolation <- interpolation
  allwaves <- seq(303, 1147, by = 1)
  
  #Linear
  if(interpolation == "linear"){
   InterCalIrr <- approx(x = data$wavelength, y = data$irradiance,
                         xout = allwaves,  method = "linear")
   InterCalRad <- approx(x = data$wavelength, y = data$radiance,
                         xout = allwaves,  method = "linear")
   InterWhite <- approx(x = data$wavelength, y = calrefl$avg,
                        xout = allwaves,  method = "linear")
  }
  
  #Spline 
  if (interpolation == "spline"){
   InterCalIrr <- spline(x = data$wavelength, y = data$irradiance,
                         xout = allwaves, method = "natural")
   InterCalRad <- spline(x = data$wavelength, y = data$radiance,
                         xout = allwaves, method = "natural")
   InterWhite <- spline(x = data$wavelength, y = calrefl$avg,
                        xout = allwaves, method = "natural")  
  }
  
  #Cubic Interpolation using the Forsythe, Malcolm and Moler method as described
  #in the R help files for the spline() function.
  #"...an exact cubic is fitted through the four points at each end of the data, 
  #and this is used to determine the end conditions"
  if (interpolation == "cubic"){
   InterCalIrr <- spline(x = data$wavelength, y = data$irradiance,
                         xout = allwaves, method = "fmm")
   InterCalRad <- spline(x = data$wavelength, y = data$radiance,
                         xout = allwaves, method = "fmm")
   InterWhite <- spline(x = data$wavelength, y = calrefl$avg,
                        xout = allwaves, method = "fmm")  
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
 
 getNumberCalFiles <- reactive({
  #Count the number of calibration files entered.
  nrow(input$calfiles)
 })
 
 getNumberEventFiles <- reactive({
  #Count the number of event files entered.
  nrow(input$eventfiles)
 })

 getCalDataList <- reactive({
  #For each calibration file entered, use the getDataFromFile function to extract
  #the data, then put each data frame into a list. The result is a list of data
  #frames, each with three columns: wavelength, radiance, and irradiance
  calfiles <- input$calfiles 
  files <- lapply(X = calfiles[,4], getDataFromFile, input$upchannel) 
  return(files)
 })
 
 getCalDataFrame <- reactive({
  #For each calibration file entered, use the getCalDataList function to get a list
  #of all the calibration data frames. Then bind them into a single data frame
  #with four columns: wavelength, radiance, irradiance, and filename.
  files <- getCalDataList()
  data <- do.call(rbind, files)
  data$filename <- as.factor(rep(input$calfiles[,1], each=256))
  return(data)
 })
 
 getEventDataList <- reactive({
  #For each event file entered, use the getDataFromFile function to extract
  #the data, then put each data frame into a list. The result is a list of data
  #frames, each with three columns: wavelength, radiance, and irradiance
  eventfiles <- input$eventfiles 
  files <- lapply(X = eventfiles[,4], getDataFromFile, input$upchannel) 
  for(i in 1:length(files)){files[[i]]$filename <- eventfiles[i,1]}
  if(input$direction == "backward"){files <- rev(files)} 
  return(files)
 })
 
 getEventDataFrame <- reactive({
  #For each event file entered, use the getEventDataList function to get a list
  #of all the event data frames. Then bind them into a single data frame
  #with four columns: wavelength, radiance, irradiance, and filename.
  files <- getEventDataList()
  data <- do.call(rbind, files)
  data$filename <- as.factor(rep(input$eventfiles[,1], each=256))
  return(data)
 })
 
 getAvgCalRefl <- reactive({
  #getAvgCalRefl: calculate the average reflectance for all of the white panel 
  #files, and return a data frame with three columns: wavelength, avg, sd
  caldata <- getCalDataList()
  paneldata <- data.frame(wavelength = caldata[[1]]$wavelength)
  
  #For each of the white panel files, read in the file, calculate reflectance, and 
  #add a column to the results file
  for(i in 1:length(caldata)){
   pfile <- caldata[[i]]
   paneldata[,i+1] <- pfile$radiance/pfile$irradiance
  }
  
  #Calculate mean reflectance and the standard deviation of the reflectance at every 
  #wavelength measured in the raw data files
  if(length(caldata) == 1){
   paneldata$avg <- paneldata[,2]
   paneldata$sd <- rep(NA, nrow(paneldata))
  } else {
   paneldata$avg <- rowMeans(paneldata[,2:ncol(paneldata)])
   paneldata$avg[which(paneldata$avg > 1000)] <- NA
   paneldata$sd <- apply(paneldata[2:(ncol(paneldata)-1)],1,sd,na.rm=TRUE)
  }
  
  avgrefldata <- data.frame(paneldata$wavelength, paneldata$avg, paneldata$sd)
  names(avgrefldata) <-c("wavelength","avg","sd")
  return(avgrefldata)
 })
 
 getEventNormRefl <- reactive({
  #getEventNormRefl: for a set of event files, calculate the normalized reflectance
  #for each file, using the average reflectance of the white panels. Return a 
  #data frame with three columns: location, wavelength, and normrefl (normal reflectance).
  calrefl <- getAvgCalRefl()
  eventdata <- getEventDataList()
  interpdata <- lapply(eventdata, getInterpNormRefl, calrefl, input$interpolation)
  normrefl <- do.call(rbind, interpdata)
  normrefl$location <- rep(1:length(eventdata),each = 845)
  normrefl <- normrefl[,c("location","wavelength","normrefl")]
  return(normrefl)
 })
 
 getEventNormReflWIrrRad <- reactive({
  #getEventNormRefl: for a set of event files, calculate the normalized reflectance
  #for each file, using the average reflectance of the white panels. Return a 
  #data frame with five columns: location, wavelength, and normrefl (normal reflectance).
  calrefl <- getAvgCalRefl()
  eventdata <- getEventDataList()
  interpdata <- lapply(eventdata, getInterpNormRefl, calrefl, input$interpolation)
  normrefl <- do.call(rbind, interpdata)
  normrefl$location <- rep(1:length(eventdata),each = 845)
  normrefl <- normrefl[,c("location","wavelength","irradiance","radiance","calrefl",
                          "refl","normrefl")]
  return(normrefl)
 })
 
 getEventAvgNormRefl <- reactive({
  #Take the data frame with all normalized reflectance values for all events (using
  #getEventNormRefl()), calculate a mean normal reflectance for each wavelength, 
  #and return a data frame with two columns: avgnormrefl and wavelength.
  data <- getEventNormRefl()
  newdata <- data.frame(tapply(X = data$normrefl, INDEX = list(data$wavelength), 
                               mean, na.rm = TRUE))
  names(newdata) <- c("avgnormrefl")
  newdata$wavelength <- as.numeric(row.names(newdata))
  return(newdata)
 })

 getIndex <- function(indexdf, indexno){ 
  #Given a data frame of indices and a row number, calculate the specified index
  data <- getEventNormRefl()
  p1 <- data$normrefl[which(data$wavelength == indexdf$w1[indexno])]
  p2 <- data$normrefl[which(data$wavelength == indexdf$w2[indexno])]
  p3 <- data$normrefl[which(data$wavelength == indexdf$w3[indexno])]
  p4 <- data$normrefl[which(data$wavelength == indexdf$w4[indexno])]
  index <- eval(parse(text = indexdf$expression_form[indexno]))
  location <- unique(data$location)
  indexdata <- data.frame(location, index)
  return(indexdata)
 }
  
 getIndices <- reactive({
  #Calculate all indices in the index list and bind into a single data frame
  indexdata <- merge(getIndex(indexlist, 1), getIndex(indexlist, 2), by = "location") 
  indexdata$date <- rep(getDate(), nrow(indexdata))
  indexdata$event <- rep(input$eventno, nrow(indexdata))
  indexdata <- indexdata[,c(1,4,5,2,3)]
  for(i in 3:nrow(indexlist)){
   indexdata <- merge(indexdata, getIndex(indexlist,i), by = "location")
  }
  names(indexdata) <- c("location","date","event_no",indexlist[,2])
   for(i in 4:ncol(indexdata)){
    indexdata[,i] <- round(as.numeric(as.character(indexdata[,i])),5)
   }
  return(indexdata)
 })
 
 summarizeIndices <- reactive({
  #Calculate the minimum, mean, and maximum values of all of the indices
  indices <- getIndices()
  mins <- as.data.frame(apply(X = indices[,4:ncol(indices)], MARGIN = 2, FUN = min, na.rm = TRUE))
  means <- as.data.frame(apply(X = indices[,4:ncol(indices)], MARGIN = 2, FUN = mean, na.rm = TRUE))
  maxs <- as.data.frame(apply(X = indices[,4:ncol(indices)], MARGIN = 2, FUN = max, na.rm = TRUE))
  sds <- as.data.frame(apply(X = indices[,4:ncol(indices)], MARGIN = 2, FUN = sd, na.rm = TRUE))
  results <- cbind(mins, means, maxs, sds)
  names(results) <- c("minimum","mean","maximum","standard_deviation")
  results.2 <- data.frame(row.names(results), results)
  names(results.2)[1] <- "index"
  return(results.2)
 })
 
 #### Metadata and database interactions ####
 
 platformInput <-  reactive({
  project_title <- input$project
  project_shortname <- sqlQuery(con, paste("SELECT shortname FROM project WHERE 
                                           title = '", input$project, "'", sep = ""))
  project_shortname <- as.character(project_shortname[1,1])
  platforms <- sqlQuery(con, paste("SELECT platform_name FROM projectplatform WHERE 
                                  project_shortname = '", project_shortname, "'", sep = ""))
  return(paste(platforms[1,]))
 })
 
 projectMetadata <- reactive({
  df <- data.frame(variable = c(
                          "Platform",
                         "Unispec serial number",
                         "Integration time (ms)",
                         "Sky condition (% cloudy)",
                         "Wind",
                         "Person who collected data (1)",
                         "Person who collected data (2)",
                         "Person who analyzed data",
                         "Date data analyzed",
                         "First timestamp for calibration files",
                         "First timestamp for event files",
                         "Calibration file names",
                         "Event file names",
                         "Number of calibration files",
                         "Number of event files",
                         "Upward facing fiber optic channel", 
                         "Event number for the day",
                         "Direction of event along transect",
                         "Wavelength interpolation method",
                         "Notes about calibration files",
                         "Notes about event files"))
    df$value <- c(
              input$platform,
              input$unispec,
              input$inttime,
              input$skycondition,
              input$wind,
              input$eventperson1,
              input$eventperson2,
              input$analysisperson,
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
              format(getCalTimestamp(), "%Y-%m-%d %H:%M:%S"),
              format(getEventTimestamp(), "%Y-%m-%d %H:%M:%S"),
              paste(input$calfiles[,1], collapse = ", "),
              paste(input$eventfiles[,1], collapse = ", "),
              length(input$calfiles[,1]),
              length(input$eventfiles[,1]),
              input$upchannel,
              input$eventno, 
              input$direction, 
              input$interpolation,
              input$calnotes,
              input$eventnotes)
  return(df)
 })

            
 #### Output simple timestamp and tables ####
  
 output$caltimestamp <- renderText({
  #output a timestamp taken from the first white panel calibration file uploaded.
  timestamp <- as.character(getCalTimestamp())
  print(timestamp)
 })

 output$eventtimestamp <- renderText({
  #output a timestamp taken from the first white panel calibration file uploaded.
  timestamp <- as.character(getEventTimestamp())
  timestamp
 })

  #### Metadata Table ####
 output$metadata <- renderDataTable({
  metadata <- projectMetadata()
   write.csv(metadata, "outputFiles/tables/metadata.csv", row.names = FALSE, append = FALSE)
   metadata
   },
   options = list(bSortClasses = TRUE))

  #### All Calculated Indices Table ####  
   output$allIndexTable <- renderDataTable({
    indices <- getIndices()
    indices <- indices[,c(1,4:ncol(indices))]
    write.csv(indices, "outputFiles/tables/indices-calculated-all.csv", row.names = FALSE, 
      append = FALSE)
    indices
     }, 
     options = list(bSortClasses = TRUE))

 #### Index List Table ####
  output$indexListTable <- renderDataTable({
    indexlist[,c(1,2,7,9)]
    write.csv(indexlist, "outputFiles/tables/indices-list.csv", row.names = FALSE, 
      append = FALSE)
    indexlist
     }, options = list(bSortClasses = TRUE, sPaginationType = "full_numbers",
    div.dataTables_wrapper = list(c("#719ba7"))))
  
  #### Summary Index Table ####
  output$summaryIndexTable <- renderDataTable({
  indices <- summarizeIndices()
  write.csv(indices, "outputFiles/tables/indices-calculated-summary.csv", 
    row.names = FALSE, append = FALSE)
  indices  
 }, options = list(bSortClasses = TRUE))
  
 #### Download Button content ####
 output$downloadMetadata <- downloadHandler(
  filename = function() {paste('metadata-',getDate(), '-event', input$eventno, '.csv', sep = '')},
  content = function(con) {write.csv(projectMetadata(), con, row.names = FALSE)}
 )
 
 output$downloadEventData <- downloadHandler(
  filename = function() {paste('eventdata-', Sys.Date(), '.csv', sep = '')},
  content = function(con) {write.csv(getEventNormReflWIrrRad(), con, row.names = FALSE)}
 )
 
 output$downloadIndexData <- downloadHandler(
  filename = function() {paste('indexdata-', Sys.Date(), '.csv', sep = '')},
  content = function(con) {write.csv(getIndices(), con, row.names = FALSE)}
 )
 
 output$downloadIndexSummaries  <- downloadHandler(
  filename = function() {paste('indexdata-summary-', getDate(), '-event', input$eventno,'.csv', sep = '')},
  content = function(con) {write.csv(summarizeIndices(), con, row.names = FALSE)}
 )

  output$downloadPdfReport <- downloadHandler(filename = "rHyperSpec_pdf_report.pdf",
    content = function(file){
      # generate PDF
      knit2pdf("rHyperSpec_pdf_report.Rnw")
      
      # copy pdf to 'file'
      file.copy("rHyperSpec_pdf_report.pdf", file)
      
      # delete generated files
      file.remove("rHyperSpec_pdf_report.pdf", "rHyperSpec_pdf_report.tex",
        "rHyperSpec_pdf_report.aux", "rHyperSpec_pdf_report.log")
    },
    contentType = "application/pdf"
  )
  
 #### Panel Plots ####
 output$calFileSlider <- renderUI({
  #output a slider bar for the panel files, to be used with 'rawcalplot'
  n <- nrow(as.data.frame(input$calfiles))
  sliderInput('calfileslider', "View irradiance and radiance by file", 
              min = 1,  max = n , value = 1, step = 1)}) 
 
 output$calWaveSlider <- renderUI({
  #output a slider bar to select the wavelength range to be used in plotting - 
  #allows a user to zoom in.
  sliderInput('cal_waverange',label = 'Wavelength range for analysis', 
              min = 305, max = 1145, value = c(400,1000), step = 10)})
 
 output$rawcalplot <- renderPlot({
  #Plot the irradiance and radiance for any given (using a slider) panel file.
   files <- getCalDataList()
   p <- ggplot(files[[input$calfileslider]], aes(x = wavelength)) + 
   geom_line(aes(y = irradiance, color = 'irradiance'))+
   geom_line(aes(y = radiance, color = 'radiance')) +
   geom_point(aes(y = irradiance, color = 'irradiance')) +
   geom_point(aes(y = radiance, color = 'radiance')) +
   scale_x_continuous(limits = input$cal_waverange) +
   scale_y_continuous(limits=c(0, 70000)) +
   ylab("") + 
   scale_colour_brewer(type = "qual", palette = 6, name = "") +
   theme(legend.position = "top")
   ggsave(filename = "outputFiles/plots/calibration_raw_single_plot.png", dpi = 300, width = 6, height = 2, 
     units = "in")
   print(p)
 })
 
 output$rawcalirrplots <- renderPlot({
  #Plot all of the irradiance for a given set of panel files
  data <- getCalDataFrame()
  p <- ggplot(data, aes(x = wavelength, y = irradiance, colour = filename)) + 
   geom_line()+
   geom_point() +
   scale_x_continuous(limits = input$cal_waverange) +
   scale_y_continuous(limits=c(0, 70000)) +
   ylab("Irradiance") + theme(legend.position="none")
   ggsave(filename = "outputFiles/plots/calibration_raw_irradiance_plot.png", dpi = 300, width = 6, height = 2, 
     units = "in")
  print(p)
 })
 
 output$rawcalradplots <- renderPlot({
  #Plot all of the radiance for a given set of panel files
  data <- getCalDataFrame()
  p <- ggplot(data, aes(x = wavelength, y = radiance, colour = filename)) + 
   geom_line()+
   geom_point() +
   scale_x_continuous(limits = input$cal_waverange) +
   scale_y_continuous(limits=c(0, 70000)) +
   ylab("Radiance") + theme(legend.position="none")
   ggsave(filename = "outputFiles/plots/calibration_raw_radiance_plot.png", dpi = 300, width = 6, height = 2, 
     units = "in")
  print(p)
 })
 
 output$calreflplot <- renderPlot({
  #Plot the average reflectance for the calibration panel
  calrefl <- getAvgCalRefl()
  p <- ggplot(calrefl, aes(x = wavelength, y = avg)) +
   geom_line(colour = 'darkblue') +
   geom_point(colour = 'darkblue') +
   scale_y_continuous(limits=c(0, 5)) +
   ylab("Avg Panel Reflectance")
   ggsave(filename = "outputFiles/plots/calibration_reflectance_plot.png", dpi = 300, width = 6, height = 2, 
     units = "in")
  print(p)
 })
 
 #### Event Plots ####
  
 output$eventFileSlider <- renderUI({
  #output a slider bar for the panel files, to be used with 'raweventplot'
  n <- nrow(as.data.frame(input$eventfiles))
  sliderInput('eventfileslider', "View irradiance and radiance by file", 
              min = 1,  max = n , value = 1, step = 1)   
 }) 
 
 output$eventWaveSlider <- renderUI({
  #output a slider bar to select the wavelength range to be used in plotting - 
  #allows a user to zoom in.
  sliderInput('event_waverange',label = 'Wavelength range for analysis', 
              min = 305, max = 1145, value = c(400,1000), step = 10)})
 
 output$raweventplot <- renderPlot({
  #Plot the irradiance and radiance for any given event file (using a slider)
  files <- getEventDataList()
  p <- ggplot(files[[input$eventfileslider]], aes(x = wavelength)) + 
   geom_line(aes(y = irradiance, color = 'irradiance'))+
   geom_line(aes(y = radiance, color = 'radiance')) +
   geom_point(aes(y = irradiance, color = 'irradiance')) +
   geom_point(aes(y = radiance, color = 'radiance')) +
   scale_x_continuous(limits = input$event_waverange) +
   ylab("") + 
   scale_colour_brewer(type = "qual", palette = 6, name = "") +
   theme(legend.position = "top")
   ggsave(filename = "outputFiles/plots/single_event_radiance_plot.png", dpi = 300, width = 6, height = 2, 
     units = "in")
  print(p)
 })
 
 output$reflmap <- renderPlot({
  #Plot a color map of the normalized reflectance for any given event file 
  #(using the same slider)
  data <- getEventNormRefl()
  p <- ggplot(data, aes(x = location, y = wavelength, fill = normrefl)) +
   geom_raster() + 
   scale_y_continuous(limits = c(400,1000)) +
   scale_fill_continuous(name="Normalized\nReflectance", limits = c(0,1)) +
   theme(legend.key.height = unit(1, "cm"))
   ggsave(filename = "outputFiles/plots/reflectance_map.png", dpi = 300, width = 6, height = 3, 
     units = "in")
  print(p)
 })
 
 output$raweventirrplots <- renderPlot({
  #Plot all of the irradiance for a given set of panel files
  data <- getEventDataFrame()
  p <- ggplot(data, aes(x = wavelength, y = irradiance, colour = filename)) + 
   geom_line()+
   geom_point() +
   scale_x_continuous(limits = input$event_waverange) +
   ylab("Irradiance") + theme(legend.position="none")
   ggsave(filename = "outputFiles/plots/event_raw_irradiance_plot.png", dpi = 300, width = 6, height = 2, 
     units = "in")
  print(p)
 })
 
 output$raweventradplots <- renderPlot({
  #Plot all of the radiance for a given set of panel files
  data <- getEventDataFrame()

  p <- ggplot(data, aes(x = wavelength, y = radiance, colour = filename)) + 
   geom_line()+
   geom_point() +
   scale_x_continuous(limits = input$event_waverange) +
   ylab("Radiance") + theme(legend.position="none")
   ggsave(filename = "outputFiles/plots/event_raw_radiance_plot.png", dpi = 300, width = 6, height = 2, 
     units = "in")
  print(p)
 })
 
 output$eventreflplot <- renderPlot({
  #Plot average normalized reflectance for the event files
  p <- ggplot(getEventAvgNormRefl(), aes(x = wavelength, y = avgnormrefl)) +
   geom_line(colour = 'darkblue') +
   geom_point(colour = 'darkblue') +
   geom_smooth() +                                 
   scale_x_continuous(limits = input$event_waverange) +
   scale_y_continuous(limits=c(0, 1)) +
   ylab("Avg. Normalized Reflectance")
   ggsave(filename = "outputFiles/plots/event_reflectance_plot.png", dpi = 300, width = 6, height = 2, 
     units = "in")
  print(p)
 })
 
 #### Single index plots ####

 output$tripleindexplot <- renderPlot({
  data <- getIndices()
  aes_mapping1 <- aes_string(x = "location", y = input$indexname1)
  aes_mapping2 <- aes_string(x = "location", y = input$indexname2)
  aes_mapping3 <- aes_string(x = "location", y = input$indexname3)
p <- ggplot(data, mapping = aes_mapping1)+
   geom_point(mapping = aes_mapping1, colour = "red") +
   geom_line(mapping = aes_mapping1, colour = "red", size = 0.5) +
   geom_smooth(mapping = aes_mapping1, method = input$smoothtype,   
               formula = y ~ x, colour = "red", size = 1.5) +
   geom_point(mapping = aes_mapping2, colour = "blue") +   
   geom_line(mapping = aes_mapping2, colour = "blue", size = 0.5) +
   geom_smooth(mapping = aes_mapping2, method = input$smoothtype,
               formula = y ~ x, colour = "blue", size = 1.5) +
   geom_point(mapping = aes_mapping3, colour = "darkgreen") +
   geom_line(mapping = aes_mapping3, colour = "darkgreen", size = 0.5) +
   geom_smooth(mapping = aes_mapping3, method = input$smoothtype,
               formula = y ~ x, colour = "darkgreen", size = 1.5) +
   ylab("Index value") +
   theme(axis.text = element_text(size=12),
         axis.title = element_text(size = 14))
  ggsave(filename = "outputFiles/plots/triple_index_plot.png", dpi = 300, width = 6, height = 3, 
    units = "in")
   tripleindexplot <- p
   print(p)
  
 })
 
 output$triplelegend <- renderPlot({
  par(oma = c(0,0,0,0), mar = c(0,0,0,0))
  plot(1:10, 1:10, bg = "transparent", bty = "n", type = "n", xlab = "", 
       ylab = "", xaxt = "n", yaxt = "n")
  legend(8,10, c(input$indexname1, input$indexname2, input$indexname3), 
         col = c("red","blue","darkgreen"), lwd = 2.5, cex = 1.4, bty = "n")
  dev.off()
 })
 
 #### Index comparison plots ####
 
 output$dynindexcompplot <- renderPlot({
  aes_mapping <- aes_string(x = input$xindex, y = input$yindex)
  data <- getIndices()
  p <- ggplot(data, mapping = aes_mapping) +
   geom_point() +
   geom_smooth(method = "lm", formula = y ~ x)
  print(p)
 })

})