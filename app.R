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


setwd('~/Google Drive/MSAN2017/SPRING_2017/MSAN-622-02_Data_and_Information_Visualization/project/data_vis_project/')
# setwd('~/workdata/data_vis/data_vis_project/')
# df <- read.csv('WEOApr2017all.xls', sep = "\t", stringsAsFactors = F, na.strings = c("NA", "", "--"))

df <- read.csv('trade.csv',stringsAsFactors = FALSE)
non_countries <- c("ACP (Africa, Caribbean and Pacific Countries)", "Africa", "Africa, CIS and Middle East", 
                   "Andean Community", "American Samoa", "APEC (Asia-Pacific Economic Cooperation)", "Aruba (the Netherlands with respect to)",
                   "Asia", "Asia excluding Hong Kong re-exports", "Asia less JPN,ANZ,CHN,NICS4, IN", "Australia and New Zealand",
                   "BRIC members", "BRICS members", "CACM (Central American Common Market)", "CARICOM (Caribbean Community)", 
                   "CEMAC (Economic and Monetary Community of Central Africa)", "Chinese Taipei", "COMESA (Common Market for Eastern and Southern Africa)",
                   "Commonwealth of Independent States (CIS)", "Czech and Slovak Fed. Rep., former", "Developing and Emerging Economies",
                   "ECCAS (Economic Community of Central African States)", "ECOWAS (Economic Community of West African States)",
                   "EFTA (European Free Trade Association)", "Europe", "Europe (Indices only)", "Europe excluding EU(28) intra-trade",
                   "European Union (12)", "European Union (15)", "European Union (27)", "European Union (28)",
                   "Four East Asian traders", "French Southern and Antarctic Territory", "French Guiana", "French Polynesia",
                   "G20 - Developed Economies", "G20 - Developed Economies excl EU", "G20 - Developing economies", "G20 Members",                                        
                   "G20 Members exc. EU incl.FR, DE,IT,UK", "GCC (Gulf Co-operation Council)", "German Dem. Rep., former",
                   "LDC (Least developed countries)", "LDC exporters of agriculture", "LDC exporters of manufactures",
                   "LDC oil exporters", "MERCOSUR (Southern Common Market)", "Middle East", "NAFTA (North American Free Trade Agreement)",
                   "Netherlands Antilles", "North America", "SADC (Southern African Development Community)", "SAFTA (South Asian Free Trade Agreement)",
                   "South Africa", "South America excluding Brazil", "South and Central America", "Switzerland (Excl. Gold)",
                   "WAEMU (West African Economic and Monetary Union)",
                   "World", "World (only indices- excluding HK RX and CH Gold)", "World excluding EU(28) intra-trade",
                   "WTO Members 2015", "WTO Members 2015 Incl. HK RX", "Developing Asia excluding Hong Kong re-exports",
                   "ASEAN (Association of South East Asian Nations)")
df <- df[!(df$Reporter_description %in% non_countries), ]
unique(df$Reporter_description)


df <- df[df$Flow_Code == 'X',c('Reporter_description','Partner_description','Value', 'Year')]
#data <- df[sample(nrow(df), 50), ]
data <- df

unique(data$Partner_code)
unique(data$Partner_description)
unique(data$Reporter_description)

##### PREVIOUS DATA #########
# metadata <- unique(df[,c(3:7)])
# metadata <- metadata[with(metadata, order(c(Subject, Units,Scale))), ]
# metadata <- unique(metadata)[-1,]
# metadata$Subject.Descriptor[metadata$Subject == "Gross domestic product per capita, current prices-U.S. dollars-Units"] <- 'GDP'
# 
# df_new <- df[,-c(4:7)]
# colnames(df_new)[4:46] <- 1980:2022
# 
# china0 <- df_new[df_new$Country =='China',]
# df.melt <- melt(df_new,id=c("Country","Region","Subject"))
# china1 <- df.melt[df.melt$Country=='China',]
# names(df.melt)[names(df.melt)=="variable"] <- "year"
# df.dcast <- dcast(df.melt, Country + year + Region ~ Subject)
# df.dcast[is.na(df.dcast)] <- 0
# 
# cols <- colnames(df.dcast)[4:47]
# 
# for (col in cols) {
#   if (any(mapply(grepl, pattern=',', x=df.dcast[,col])))
#   {df.dcast[,col]<- as.numeric(gsub(",","",df.dcast[,col]))}
#   else {df.dcast[,col] <- as.numeric(df.dcast[,col])}
# }
# 
# df.dcast$year <- as.numeric(levels(df.dcast$year)[df.dcast$year])
# 
# china2 <- df.dcast[df.dcast$Country == 'China',]
# 
# # Remove less important columns
# 
# skip = c(                                                                                             
#   "GDP corresponding to fiscal year, current prices-National currency-Billions",                 
#   "GDP per capita, constant prices-National currency-Units",                                              
#   "GDP per capita, current prices-National currency-Units",                                                 
#   "GDP, constant prices-National currency-Billions",                                                          
#   "GDP, current prices-National currency-Billions",                                                     
#   "General government gross debt-National currency-Billions",                        
#   "General government net debt-National currency-Billions",                      
#   "General government net lending/borrowing-National currency-Billions",          
#   "General government primary net lending/borrowing-National currency-Billions",            
#   "General government revenue-National currency-Billions",                              
#   "General government structural balance-National currency-Billions",                               
#   "General government total expenditure-National currency-Billions",                                        
#   "Implied PPP conversion rate-National currency per current international dollar-"                          
# )
# 
# df.dcast <- df.dcast[, !(names(df.dcast) %in% skip)]

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

# Data source: http://goo.gl/vcKo6y
UKvisits <- data.frame(origin=c(
  "France", "Germany", "USA",
  "Irish Republic", "Netherlands",
  "Spain", "Italy", "Poland",
  "Belgium", "Australia", 
  "Other countries", rep("UK", 5)),
  visit=c(
    rep("UK", 11), "Scotland",
    "Wales", "Northern Ireland", 
    "England", "London"),
  weights=c(
    c(12,10,9,8,6,6,5,4,4,3,33)/100*31.8, 
    c(2.2,0.9,0.4,12.8,15.5)))



#sankey examples
require(googleVis)
plot(
  gvisSankey(UKvisits, from="origin", 
             to="visit", weight="weight",
             options=list(
               height=250,
               sankey="{link:{color:{fill:'lightblue'}}}"
             ))
)

require(igraph)
require(googleVis)
g <- graph.tree(24, children = 4)
set.seed(123)
E(g)$weight = rpois(23, 4) + 1
edgelist <- get.data.frame(g) 
colnames(edgelist) <- c("source","target","value")
edgelist$source <- LETTERS[edgelist$source]
edgelist$target <- LETTERS[edgelist$target]

plot(
  gvisSankey(edgelist, from="source", 
             to="target", weight="value",
             options=list(
               sankey="{link: {color: { fill: '#d799ae' } },
               node: { width: 4, 
               color: { fill: '#a61d4c' },
               label: { fontName: 'Times-Roman',
               fontSize: 14,
               color: '#871b47',
               bold: true,
               italic: true } }}"))
             )


#Sankey plot example from our data

# data2 <- data[!(data$Reporter_code %in% unique(data$Partner_code)) & data$Year == '2010', c('Reporter_code','Partner_code','Value')]
# data2 <- data[!(data$Partner_description %in% unique(data$Reporter_description)) & data$Year == '2010' & data$Partner_description != 'World', c('Reporter_description','Partner_description','Value')]
# data2 <- data[!(data$Report_description %in% unique(data$Partner_description)) & data$Year == '2010' & data$Partner_description != 'World', c('Reporter_description','Partner_description','Value')]
data2 <- data[!(data$Partner_description %in% unique(data$Reporter_description)) & data$Year == '2012' & data$Partner_description != 'World', ]
data3 <- data2[order(-data2$Value), ]
data4 <- data3[1:50, ]

sk1 <- gvisSankey(data2, from="From", to="To", weight="Weight")
plot(sk1)



data <- data[-7,c('Reporter_code','Partner_code','Value')]

sk2 <- gvisSankey(data, from="Reporter_code", to="Partner_code", weight="Value",
                  options=list(sankey="{link: {color: { fill: '#d799ae' } },
                               node: { color: { fill: '#a61d4c' },
                               label: { color: '#871b47' } }}"))
plot(sk2)

typeof(data$Reporter_code)

typeof(data$Partner_code)

typeof(data$Value)
