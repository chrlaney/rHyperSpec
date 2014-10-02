#ui.R
#For rHyperSpec_shinyapp


shinyUI(pageWithSidebar(

 headerPanel("rHyperSpec"),
 
 sidebarPanel( 
  
  tags$head(
   tags$style(type='text/css', ".well { max-width: 200px; }"),
   tags$style(type='text/css', ".span4 { max-width: 270px; }")
  ),
  
   fileInput('calfiles','Select calibration files',
    multiple = TRUE,
     accept = NULL),
   
   fileInput('eventfiles','Select event files',
             multiple = TRUE,
             accept = '.spu'),
   
  downloadButton('downloadMetadata', 'Download Metadata'),
  
  downloadButton('downloadEventData', 'Download Event Data'),
  
  downloadButton('downloadIndexData', 'Download Index Data'),
  
  downloadButton('downloadIndexSummaries', 'Download Index Summaries'),
  
  downloadButton('downloadPdfReport', 'Download PDF Report')
 ),
   
 mainPanel(
  tabsetPanel(
  
   tabPanel('Metadata',
            div(class="row-fluid",
                div(class="span3", 
                  selectInput("samplingpath", 
                        label = "Sampling Path",
                        choices = samplingpath)),
                div(class = "span1",""),
                div(class="span3",
                  selectInput('unispec',
                    label = "Unispec serial #\n ",
                    choices = c("2011","2012","2013"),
                    selected = c("2012"))),
                div(class = "span1",""),
                div(class="span3",
                  selectInput('upchannel', 
                    label = 'Upward fiber optic channel',
                    choices = c('Channel A' = 'A', 'Channel B' = 'B')))),
            
                 
            div(class = "row-fluid",
                div(class = "span3",
                      numericInput('inttime',
                        label = "Integration time (ms)",
                        value = 10)),
                div(class = "span1",""),
                div(class = "span3",
                    numericInput('intfileno',
                      label = "Number files per integration",
                      value = 10)),
                div(class = "span1",""),
                div(class="span3",
                  selectInput('eventno', 
                    label = 'Event number for the day',
                    choices = c('1' = '1', '2' = '2', '3' = '3', '4' = '4',
                      '5' = '5', '6' = '6', '7' = '7', '8' = '8',
                      '9' = '9', '10' = '10')))),

            
            div(class = "row-fluid",
                div(class = "span3",
                      selectInput('eventperson1', 
                        label = 'Data collector #1',
                        choices = people)),
                div(class = "span1",""),
                div(class = "span3",
                  selectInput('eventperson2', 
                    label = 'Data collector #2',
                    choices = people)),
              div(class = "span1",""),
              div(class="span3",
                selectInput('analysisperson', 
                  label = 'Data analyzer',
                  choices = people))),

   div(class = "row-fluid",         
    div(class = "span3",
          selectInput('skyclasscondition',
                     label = "Sky condition by class",
                     choices = c("0% (No clouds)", "0-10% (Clear)", "10-25% (Isolated)",
                       "25-50% (Scattered)", "50-90% (Broken)", "90-100% (Overcast)"))),
    div(class = "span1",""),
    div(class = "span3",
         textInput('skycondition',
         label = "Sky condition % cloudy",
         value = "0")),
     div(class = "span1",""),
     div(class = "span3",
          selectInput('wind', 
                            label = 'Wind',
                            choices = c('negligable' = 'negligible', 
                              'breezy' = 'breezy','windy' = 'windy', 
                              'extremely windy' = 'extremely windy')))),
 

            div(class="row-fluid",
                div(class = "span3",
                      numericInput("sunanglestart", 
                                    "Sun Angle (start)",
                                    value = NULL)),
                div(class = "span1",""),
                div(class = "span3",
                  numericInput("sunangleend", 
                    "Sun Angle (end)",
                    value = NULL))),
     
     div(class="row-fluid",
       div(class = "span5",
         textInput('tracknotes',
           label = "Track condition notes",
           value = ""),
        tags$head(tags$style(type="text/css", "#tracknotes {width: 350px}"))),
       div(class = "span1",""),
       div(class = "span5",
           textInput('instrumentnotes',
             label = "Instrumentation notes",
             value = ""),
     tags$head(tags$style(type="text/css", "#instrumentnotes {width: 350px}")))),
     

     div(class="row-fluid",
       div(class = "span5",
         textInput('weathernotes',
           label = "Weather notes",
           value = ""),
         tags$head(tags$style(type="text/css", "#weathernotes {width: 350px}"))),
       div(class = "span1",""),
       div(class = "span5",
           textInput('datanotes',
             label = "Data file or collection notes",
             value = ""),
         tags$head(tags$style(type="text/css", "#datanotes {width: 350px}")))),

     HTML('<hr style="background:#F87431; border:0; height:5px" />'),
            div(class="row-fluid",
              div(class = "span3",
                radioButtons("direction", "Direction files given:",
                             c("Forward" = "forward", "Backward" = "backward"))),
              div(class = "span1",""),
              div(class = "span3",
                radioButtons("interpolation", "Interpolation type:",
                               c("Linear" = "linear", "Spline" = "spline", 
                                 "Cubic" = "cubic"))),
              div(class = "span1",""),
              div(class = "span3",
                radioButtons("limitNR", "Normalized Reflectance Calculation Options:",
                  c("Change values > 1 to 1 and < -1 to -1" = "lim1", 
                    "Change values > 1 and < -1 to NA" = "limNA",
                    "No Limits" = "nl")))), 
     HTML('<hr style="background:#F87431; border:0; height:5px" />'),
     dataTableOutput('metadata')
            ),

    tabPanel('Index List',
      dataTableOutput('indexListTable')),
 
   tabPanel('Calibration Plots',
            tags$head(tags$style(type="text/css", ".jslider {max-width: 500px; }")),
            uiOutput('calWaveSlider'),
            plotOutput('rawcalirrplots', height = "180px"),
            plotOutput('rawcalradplots', height = "180px"),
            uiOutput('calFileSlider'),
            plotOutput('rawcalplot', height = '230px')),
            
   tabPanel('Event Plots',
            tags$head(tags$style(type="text/css", ".jslider {max-width: 500px; }")),
            uiOutput('eventWaveSlider'),
            plotOutput('raweventirrplots', height = "180px"),
            plotOutput('raweventradplots', height = "180px"),
            uiOutput('eventFileSlider'),
            plotOutput('raweventplot', height = '230px')),
            
   tabPanel('Reflectance Plots',      
            plotOutput('calreflplot', height = "180px"),
            plotOutput('eventreflplot', height = "180px"),
            plotOutput('reflmap', height = "300px")),

   tabPanel('Table of Calculated Indices',
            dataTableOutput('allIndexTable')), 
   
   tabPanel('Table of Average Index Values',
            dataTableOutput('summaryIndexTable')), 
   
   tabPanel('Index Plots',
            div(class="row-fluid",
                div(class="span3", 
                    selectInput(inputId = 'indexname1', label = 'Index 1:', 
                                choices = indexlist[,2], selected = 'ndvi1')),
                div(class="span3",
                    selectInput(inputId = 'indexname2', label = 'Index 2:', 
                                choices = indexlist[,2], selected = 'pri1')),
                div(class="span3",
                    selectInput(inputId = 'indexname3', label = 'Index 3:', 
                                choices = indexlist[,2], selected = 'wbi'))
                ),
            selectInput('smoothtype', 'Smoothing Method:',
                        choices = c('loess','lm','glm','rlm','gam'), 
                        selected = 'loess'),
            plotOutput('triplelegend', height = "70px"),
            plotOutput('tripleindexplot', height = "400px")),
           
   
   tabPanel('Index Comparison Plots',
            selectInput('xindex', 'X-axis Index:',
                        indexlist[,2], selected = 'green1'),
            selectInput('yindex', 'Y-axis Index:',
                        indexlist[,2], 'ndvi1'),
             plotOutput('dynindexcompplot'))           
  ))
))