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
  
  downloadButton('downloadIndexSummaries', 'Download Index Summaries')
  
 ),
   
 mainPanel(
  tabsetPanel(
  
   tabPanel('Metadata',
            div(class="row-fluid",
                div(class="span3", 
                  selectInput('platform', 
                        label = 'Platform',
                        choices = c("SEL-Jornada Tramline",
                                    "SEL-Biocomplexity Control Tramline",
                                    "SEL-Biocomplexity Drained Tramline",
                                    "SEL-Biocomplexity Flooded Tramline"))),
                div(class = "span1",""),
                div(class="span3",
                  selectInput('eventperson1', 
                        label = 'Data collector #1',
                        choices = c("Craig Tweedie","Christine Laney","Sergio Vargas",
                                    "Santonu Goswami","Aline Jaimes","Naomi Luna",
                                    "Loren Ochoa","Gesuri Ramirez","Jose Herrera"))),
                div(class = "span1",""),
                div(class="span3",
                    textInput('calnotes',
                              label = "Notes about calibration files",
                              value = ""))),
            
                 
            div(class = "row-float",
                div(class = "span3",
                    selectInput('unispec',
                                label = "Unispec serial #\n ",
                                choices = c("2011","2012","2013"),
                                selected = c("2012"))),
                div(class = "span1",""),
                div(class = "span3",
                    selectInput('eventperson2', 
                                label = 'Data collector #2',
                                choices = c("None","Craig Tweedie","Christine Laney","Sergio Vargas",
                                            "Santonu Goswami","Aline Jaimes","Naomi Luna",
                                            "Loren Ochoa","Gesuri Ramirez","Jose Herrera"))),
                div(class = "span1",""),
                div(class="span3",
                    textInput('eventnotes',
                              label = "Notes about the event files",
                              value = ""))),

            
            div(class = "row-fluid",
                div(class = "span3",
                    selectInput('upchannel', 
                                label = 'Upward fiber optic channel',
                                choices = c('Channel A' = 'A', 'Channel B' = 'B'))),
                div(class = "span1",""),
                div(class = "span3",
                    selectInput('analysisperson', 
                                label = 'Data analyzer',
                                choices = c("Craig Tweedie","Christine Laney","Sergio Vargas",
                                            "Santonu Goswami","Aline Jaimes","Naomi Luna",
                                            "Loren Ochoa","Gesuri Ramirez","Jose Herrera")))),

   div(class = "row-fluid",         
    div(class = "span3",
                numericInput('inttime',
                             label = "Integration time (ms)",
                             value = 10)),
           div(class = "span1",""),
           div(class = "span3",
           textInput('skycondition',
                     label = "Sky condition (% cloudy)",
                     value = "0"))),
 

            div(class="row-fluid",
                div(class = "span3",
                    selectInput('eventno', 
                                label = 'Event number for the day',
                                choices = c('1' = '1', '2' = '2', '3' = '3', '4' = '4',
                                            '5' = '5', '6' = '6', '7' = '7', '8' = '8',
                                            '9' = '9', '10' = '10'))),
                div(class = "span1",""),
                div(class = "span3",
                selectInput('wind', 
                            label = 'Wind',
                            choices = c('negligable' = 'negligable', 'breezy' = 'breezy','windy' = 'windy', 'extremely windy' = 'extremely windy')))),
   

            div(class="row-fluid",
                div(class = "span3",
                    radioButtons("direction", "Direction files given:",
                                 c("Forward" = "forward", "Backward" = "backward"))),
                    div(class = "span1",""),
                    div(class = "span3",
                        radioButtons("interpolation", "Interpolation type:",
                                     c("Linear" = "linear", "Spline" = "spline", 
                                       "Cubic" = "cubic")))),
            htmlOutput('metadata')
            ),
   
#   if(online == TRUE){
   tabPanel('Index List',
     htmlOutput('indexListTable')), #} else {
#       tabPanel('Index List',
#     tableOutput('indexListTable'))
#  },
  
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

#   if(online==TRUE){
     tabPanel('Table of Calculated Indices',
              htmlOutput('allIndexTable')), #} else {
#                tabPanel('Table of Calculated Indices',
#                tableOutput('allIndexTable'))
#            },
   
#   if(online==TRUE){
   tabPanel('Table of Average Index Values',
            htmlOutput('summaryIndexTable')), #} else {
#              tabPanel('Table of Average Index Values',
#              tableOutput('summaryIndexTable'))
#            },
   

   
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