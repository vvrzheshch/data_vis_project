# Data-viz project
# Alice Zhao, Valentin Vrzheshch
# shiny::runGitHub('data_vis_project', 'vvrzheshch')

rm(list = ls())
cat("\014")

library(shiny)
library(tidyr)
library(reshape2)
# library(plyr)


# setwd('~/Google Drive/MSAN2017/SPRING_2017/MSAN-622-02_Data_and_Information_Visualization/project/data_vis_project/')
df <- read.csv('WEOApr2017all.xls', sep = "\t", stringsAsFactors = F, na.strings = c("NA", "", "--"))

df.melt <- melt(df, id=c("Country", "Subject.Descriptor"))
# df.melt <- cast(df, id=c("Country", "Subject.Descriptor"))
# df.dcast <- dcast(df.melt, value ~ Country + Subject.Descriptor + variable) 

df.melt$value <- as.numeric(df.melt$value)
# df.melt$id <- 1:nrow(df.melt)
names(df.melt)[names(df.melt)=="variable"] <- "year"

df.dcast <- dcast(df.melt, Country + year ~ Subject.Descriptor, value.var="value", fun.aggregate = sum)
df.dcast$year <- as.numeric(gsub("[^0-9\\.]", "", df.dcast$year)) 
# df.dcast <- dcast(df.melt, Country + year ~ Subject.Descriptor, value.var="value")


# df.wide <- spread(df.melt, Subject.Descriptor, value)
# df.wide$year <- as.numeric(gsub("[^0-9\\.]", "", df.wide$year)) 
# df.wide <- spread(df, c('Country', 'Subject.Descriptor', 'Units', 'Scale'), value)


# df.melt <- melt(df, id=c("Country", "Subject.Descriptor", "Subject.Notes", "Units", "Scale"))
# # df.melt <- melt(df, id=c("Country", "Subject.Notes", "Units", "Scale"))
# df.cast <- cast(df.melt, Country + Units  ~ Subject.Descriptor )

library(ggplot2)
library(GGally)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
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
        plotlyOutput("Bubble.Plot"),
        plotOutput("par_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  sub_df <- reactive({df.dcast[df.dcast$year == input$year, ]})
   
   output$Bubble.Plot <- renderPlotly({
     plot_ly(sub_df(), x = ~get(input$x), y = ~get(input$y), color = ~Country, size=~Population) %>%
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
     df_2 <- na.omit(sub_df()[, c("Country", "Population", "Gross national savings", "General government total expenditure", "Population")])
     # ggparcoord(data = df_2, scale = 'uniminmax', groupColumn = "Country", scaleSummary = "mean")
     ggparcoord(data = df_2, scale = 'uniminmax', scaleSummary = "mean", splineFactor = 10)#, groupColumn = "Country")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

