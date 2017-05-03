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

# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
library(googleCharts)
# library(plyr)


# setwd('~/Google Drive/MSAN2017/SPRING_2017/MSAN-622-02_Data_and_Information_Visualization/project/data_vis_project/')
df <- read.csv('WEOApr2017all.xls', sep = "\t", stringsAsFactors = F, na.strings = c("NA", "", "--"))

df.melt <- melt(df[, !(names(df) %in% c('Region', "Subject.Notes",      "Units", "Scale"))],
                id=c("Country", "Subject.Descriptor"))

# df.melt <- cast(df, id=c("Country", "Subject.Descriptor"))
# df.dcast <- dcast(df.melt, value ~ Country + Subject.Descriptor + variable) 

df.melt$value <- as.numeric(df.melt$value)
# df.melt$id <- 1:nrow(df.melt)
names(df.melt)[names(df.melt)=="variable"] <- "year"

df.dcast <- dcast(df.melt, Country + year ~ Subject.Descriptor, value.var="value", fun.aggregate = sum)
df.dcast$year <- as.numeric(gsub("[^0-9\\.]", "", df.dcast$year)) 
# df.dcast <- dcast(df.melt, Country + year ~ Subject.Descriptor, value.var="value")

df.final <- merge(df.dcast, unique(df[, c('Country', 'Region')]), all.x=T, by.x = 'Country', by.y = 'Country')
colnames(df.final)[which(names(df.final) == "Gross domestic product based on purchasing-power-parity (PPP) per capita GDP")] <- "GDP"

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Data Viz Project VV & AZ"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     h3("Choose year:"),
                     min = min(df.dcast$year, na.rm = T),
                     max = max(df.dcast$year, na.rm = T),
                     value = 2005, step=1, animate = T),
         selectInput("x", label = h3("Select x-axis:"), choices = unique(df$Subject.Descriptor), selected = 'Population'),
         selectInput("y", label = h3("Select y-axis:"), choices = unique(df$Subject.Descriptor), selected = "Employment")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("bubble plot",plotlyOutput("Bubble.Plot")),
          tabPanel("parallel coordinates plot", plotOutput("par_plot")),
          tabPanel("Google Chart",
                   # This line loads the Google Charts JS library
                   # googleChartsInit(chartTypes = 'sankey'), 
                   googleChartsInit(),
                   #   googleChartsInit(chartTypes = c("ALL", "annotatedtimeline", "area", "bar", "bubble", "calendar", "candlestick", "column", "combo", "gauge", "geo", "geomap", "intensitymap", "line", "map", "motion", "org", "pie", "sankey", "scatter", "steppedarea", "table", "timeline", "treemap"))
                   googleBubbleChart("google_chart", width = '100%', height='600px',
                         options = list(fontName = "Comic Sans", fontSize = 11
                        )
                  )
          )
       )
     )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  sub_df <- reactive({df.final[df.final$year == input$year, ]})
   
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
     df_2 <- na.omit(sub_df()[, c("Region", "Country", "Population",  "Employment", "Gross national savings", "Gross domestic product, constant prices","Total investment")])
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
}

# Run the application 
shinyApp(ui = ui, server = server)

