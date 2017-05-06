# Data-viz project
# Alice Zhao, Valentin Vrzheshch
# shiny::runGitHub('data_vis_project', 'vvrzheshch')

rm(list = ls())
cat("\014")

library(shiny)
library(tidyr)
library(reshape2)
library(directlabels)
library(ggthemes)
library(plotly)
library(ggplot2)
library(GGally)
library(googleVis)

# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
library(googleCharts)
# library(plyr)


#setwd('~/Google Drive/MSAN2017/SPRING_2017/MSAN-622-02_Data_and_Information_Visualization/project/data_vis_project/')
setwd('~/workdata/data_vis/data_vis_project/')
df <- read.csv('WEOApr2017all.xls', sep = "\t", stringsAsFactors = F, na.strings = c("NA", "", "--"))


metadata <- unique(df[,c(3:7)])
metadata <- metadata[with(metadata, order(c(Subject, Units,Scale))), ]
metadata <- unique(metadata)[-1,]
metadata$Subject.Descriptor[metadata$Subject == "Gross domestic product per capita, current prices-U.S. dollars-Units"] <- 'GDP'

df_new <- df[,-c(4:7)]
colnames(df_new)[4:46] <- 1980:2022

china0 <- df_new[df_new$Country =='China',]
df.melt <- melt(df_new,id=c("Country","Region","Subject"))
#df.melt$value <- as.numeric(df.melt$value)
china1 <- df.melt[df.melt$Country=='China',]
names(df.melt)[names(df.melt)=="variable"] <- "year"
df.dcast <- dcast(df.melt, Country + year + Region ~ Subject)
#df.dcast[,4:47] <- sub(",","",df.dcast[,4:47])
#df.dcast$Population <- as.numeric(gsub(",","",df.dcast$Population))
df.dcast[is.na(df.dcast)] <- 0

#colnames(df.dcast)[4:47] <- metadata$Subject.Descriptor
cols <- colnames(df.dcast)[4:47]
#sub(",","",df.dcast[,4:47])

for (col in cols) {
  #print (col)
  if (any(mapply(grepl, pattern=',', x=df.dcast[,col])))
  {df.dcast[,col]<- as.numeric(gsub(",","",df.dcast[,col]))}
  else {df.dcast[,col] <- as.numeric(df.dcast[,col])}
}

df.dcast$year <- as.numeric(levels(df.dcast$year)[df.dcast$year])
#as.integer(as.character(df.dcast$Year))

china2 <- df.dcast[df.dcast$Country == 'China',]

# Remove less important columns

skip = c(                                                                                             
  "GDP corresponding to fiscal year, current prices-National currency-Billions",                 
  "GDP per capita, constant prices-National currency-Units",                                              
  "GDP per capita, current prices-National currency-Units",                                                 
  "GDP, constant prices-National currency-Billions",                                                          
  "GDP, current prices-National currency-Billions",                                                     
  "General government gross debt-National currency-Billions",                        
  "General government net debt-National currency-Billions",                      
  "General government net lending/borrowing-National currency-Billions",          
  "General government primary net lending/borrowing-National currency-Billions",            
  "General government revenue-National currency-Billions",                              
  "General government structural balance-National currency-Billions",                               
  "General government total expenditure-National currency-Billions",                                        
  "Implied PPP conversion rate-National currency per current international dollar-"                          
)

df.dcast <- df.dcast[, !(names(df.dcast) %in% skip)]

# Define UI for application that draws a histogram
ui <- fluidPage(
     # Application title
   titlePanel("Data Viz Project VV & AZ"),
     mainPanel(
       tabsetPanel(
         tabPanel("bubble plot",plotlyOutput("Bubble.Plot")),
         tabPanel("parallel coordinates plot", plotOutput("par_plot")),
         tabPanel("Google Chart",
                  # This line loads the Google Charts JS library
                  # googleChartsInit(chartTypes = 'sankey'), 
                  googleChartsInit(),
                  
                  #googleChartsInit(chartTypes ="geomap")
                  #   googleChartsInit(chartTypes = c("ALL", "annotatedtimeline", "area", "bar", "bubble", "calendar", "candlestick", "column", "combo", "gauge", "geo", "geomap", "intensitymap", "line", "map", "motion", "org", "pie", "sankey", "scatter", "steppedarea", "table", "timeline", "treemap"))
                  googleBubbleChart("google_chart", width = '900px', height='600px',
                                    options = list(fontName = "Comic Sans", fontSize = 11),  
                                    chartArea = list(
                                      top = 5, left = 7,
                                      height = "75%", width = "100%")
                  )),
         tabPanel("google map", htmlOutput("map"))
         ) 
       )
   ,
     fluidRow(
       shiny::column(4, offset = 4,
                     sliderInput("year", "Year",
                                 min = min(df.dcast$year), max = max(df.dcast$year),
                                 value = min(df.dcast$year), animate = TRUE),
           selectInput("x", label = h3("Select x-axis:"), choices = names(df.dcast), selected = 'Population'), #choices = unique(df$Subject)
           selectInput("y", label = h3("Select y-axis:"), choices = names(df.dcast), selected = "Employment")
       )
     )
)
      # sidebarPanel(
     # wellPanel(
     #     sliderInput("year",
     #                 h3("Choose year:"),
     #                 min = min(df.dcast$year, na.rm = T),
     #                 max = max(df.dcast$year, na.rm = T),
     #                 value = 2005, step=1, animate = T),
     #     selectInput("x", label = h3("Select x-axis:"), choices = unique(df$Subject.Descriptor), selected = 'Population'),
     #     selectInput("y", label = h3("Select y-axis:"), choices = unique(df$Subject.Descriptor), selected = "Employment")
     #  )



# Define server logic required to draw a histogram
server <- function(input, output) {
  sub_df <- reactive({df.dcast[df.dcast$year == input$year, ]})
   
   output$Bubble.Plot <- renderPlotly({
     plot_ly(sub_df(), x = ~get(input$x), y = ~get(input$y), color = ~Region, size=~Population) %>%
     layout(xaxis =  list(title = input$x, showgrid = T), yaxis = list(title = input$y, showgrid = T)) 
     # , colors = colors,
     #              type = 'scatter', mode = 'markers', sizes = c(min(data_2007$size), max(data_2007$size)),
     #              marker = list(symbol = 'circle', sizemode = 'diameter',
     #                            line = list(width = 2, color = '#FFFFFF')),
     #              text = ~paste('Country:', country, '<br>Life Expectancy:', lifeExp, '<br>GDP:', gdpPercap,
     #                            '<br>Pop.:', pop)) %>%
     #   layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
     #          xaxis = list(title = 'GDP per capita (2000 dollars)',
     #                       gridcolor = 'rgb(255, 255, 255)',
     #                       range = c(2.003297660701705, 5.191505530708712),
     #                       type = 'log',
     #                       zerolinewidth = 1,
     #                       ticklen = 5,
     #                       gridwidth = 2),
     #          yaxis = list(title = 'Life Expectancy (years)',
     #                       gridcolor = 'rgb(255, 255, 255)',
     #                       range = c(36.12621671352166, 91.72921793264332),
     #                       zerolinewidth = 1,
     #                       ticklen = 5,
     #                       gridwith = 2),
     #          paper_bgcolor = 'rgb(243, 243, 243)',
     #          plot_bgcolor = 'rgb(243, 243, 243)')
   })
   
   output$par_plot <- renderPlot({
     df_2 <- na.omit(sub_df()[, c("Region", "Country", "Population", "Employment", "Gross national savings-Percent of GDP-", "GDP, current prices-USD-Billions","Total investment-Percent of GDP-")])
     # colnames(df_2)[5] <- 'GDP'
     # ggparcoord(data = df_2, scale = 'uniminmax', groupColumn = "Country", scaleSummary = "mean")
     ggparcoord(data = df_2, columns = 3:ncol(df_2), scale = 'uniminmax', scaleSummary = "mean", splineFactor = F, groupColumn = "Region") +
       geom_dl(aes(label = Country), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +  
       theme_gdocs() + scale_color_gdocs(guide=FALSE) + 
       theme(axis.title = element_blank())
   })
  
  output$google_chart <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
  list(
    data = googleDataTable(sub_df()[, c('Country', input$x, input$y, 'Region', 'Population')]),
    options = list(
      title = paste0(input$x, " vs. ", input$y, " for year ", input$year),
      # Set axis labels and ranges
      hAxis = list(title = input$x, viewWindow = xlim),
      vAxis = list(title = input$y, viewWindow = ylim)
      # title = "Panda"
      # series = series
    )
  )
    # sub_df()[, c('Country', input$x, input$y, 'Country', 'Population')]

   })
  output$map <- renderGvis({
    # subset <- df.dcast[df.dcast$year == input$year,c("Country","Population")]
    map <- gvisGeoChart(sub_df(), locationvar="Country", 
                   colorvar=input$x,
                   options=list(projection="kavrayskiy-vii", displayMode="regions",height = 600, width = 900))
    return(map)})
}



# china <- df.dcast[df.dcast$Country == 'China',]
# 
# subset <- df.dcast[df.dcast$year == 2003,c("Country","Population")]
# Geo=gvisGeoChart(subset, locationvar="Country", 
#                  colorvar="Population",options=list(projection="kavrayskiy-vii",displayMode="regions"))
# plot(Geo)
# 
# Run the application 
shinyApp(ui = ui, server = server)
