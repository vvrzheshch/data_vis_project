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
library(data.table)
library(GGally)
library(googleVis)
library(googleCharts) # More info:  https://github.com/jcheng5/googleCharts Install: devtools::install_github("jcheng5/googleCharts")
library(plyr)
library(treemap)
library(d3treeR)
library(shinydashboard) # http://fontawesome.io/icons/


setwd('~/Google Drive/MSAN2017/SPRING_2017/MSAN-622-02_Data_and_Information_Visualization/project/data_vis_project/')
# setwd('~/workdata/data_vis/data_vis_project/')

df <- read.csv('trade.csv',stringsAsFactors = FALSE)
df <- df[df$Year >= 2000, ]

# Convert names to ASCII
df$Reporter_description <-iconv(df$Reporter_description, "latin1", "ASCII", "")  

# Prepare data for Tree Map
  df_tree <- df[df$Indicator_description != "Total merchandise", c("Reporter_description", "Indicator_description", "Flow_Description", "Year", "Value")]
  df_tree <- aggregate(Value ~ Reporter_description + Indicator_description + Flow_Description + Year,
                       df_tree, sum)
  

# Prepare data for Sankey plot
  df_export <- df[df$Flow_Code == 'X' & df$Indicator_description == "Total merchandise" & df$Partner_description != 'World', ]
  df_import <- df[df$Flow_Code == 'M' & df$Indicator_description == "Total merchandise" & df$Partner_description != 'World', ]
  colnames(df_import)[1:4] <- c("Partner_code","Partner_description","Reporter_code","Reporter_description")
  df_sankey <-  aggregate(Value~Reporter_description+Partner_description+Year,
                              rbind(df_export,df_import),
                              sum)
  non_countries <- c("World", "Extra-trade", "Other Africa", "Other Europe", "Other Asia", "Africa", "Europe", "ACP (Africa, Caribbean and Pacific Countries)", "Africa, CIS and Middle East", "Andean Community", "American Samoa", "APEC (Asia-Pacific Economic Cooperation)", "Aruba (the Netherlands with respect to)", "Asia", "Asia excluding Hong Kong re-exports", "Asia less JPN,ANZ,CHN,NICS4, IN", "Australia and New Zealand", "BRIC members", "BRICS members", "CACM (Central American Common Market)", "CARICOM (Caribbean Community)",  "CEMAC (Economic and Monetary Community of Central Africa)", "Chinese Taipei", "COMESA (Common Market for Eastern and Southern Africa)", "Commonwealth of Independent States (CIS)", "Czech and Slovak Fed. Rep., former", "Developing and Emerging Economies", "ECCAS (Economic Community of Central African States)", "ECOWAS (Economic Community of West African States)", "EFTA (European Free Trade Association)", "Europe (Indices only)", "Europe excluding EU(28) intra-trade", "European Union (12)", "European Union (15)", "European Union (27)", "European Union (28)", "Four East Asian traders", "French Southern and Antarctic Territory", "French Guiana", "French Polynesia", "G20 - Developed Economies", "G20 - Developed Economies excl EU", "G20 - Developing economies", "G20 Members", "G20 Members exc. EU incl.FR, DE,IT,UK", "GCC (Gulf Co-operation Council)", "German Dem. Rep., former", "LDC (Least developed countries)", "LDC exporters of agriculture", "LDC exporters of manufactures", "LDC oil exporters", "MERCOSUR (Southern Common Market)", "Middle East", "NAFTA (North American Free Trade Agreement)", "Netherlands Antilles", "North America", "SADC (Southern African Development Community)", "SAFTA (South Asian Free Trade Agreement)", "South Africa", "South America excluding Brazil", "South and Central America", "Switzerland (Excl. Gold)", "WAEMU (West African Economic and Monetary Union)", "World", "World (only indices- excluding HK RX and CH Gold)", "World excluding EU(28) intra-trade", "WTO Members 2015", "WTO Members 2015 Incl. HK RX", "Developing Asia excluding Hong Kong re-exports", "ASEAN (Association of South East Asian Nations)",'Other CIS (CIS11)')
  non_countries_2 <- paste0(non_countries,' ')
  # df_sankey <- na.omit(df_sankey)
  df_sankey$Partner_description <- paste0(df_sankey$Partner_description,' ')
  # df_sankey <- df_sankey[order(df_sankey$Reporter_description), ]

  
ui <- dashboardPage(
    dashboardHeader(title = "World Trade"),
    dashboardSidebar(
      sidebarMenu(id = "sidebarmenu",
        # http://fontawesome.io/icons/
        menuItem("Sankey Plot", tabName = "sankey", icon = icon("exchange")),
        conditionalPanel("input.sidebarmenu === 'sankey'",
                         radioButtons("radio.left", "Left:", c("Countries" = "Countries", "Regions" = "Regions"), selected="Countries"),
                         radioButtons("radio.right", "Right:", c("Countries" = "Countries", "Regions" = "Regions"), selected="Regions")
          ),
        menuItem("Map", tabName = "globe", icon = icon("globe")),
        menuItem("Tree Map", tabName = "treemap", icon = icon("columns")),
        sliderInput("year", "Year", min = min(df$Year), max = max(df$Year), value = max(df$Year)-5, animate = TRUE, step = 1, sep = "")
      )
    ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "sankey",
                fluidRow(
                  box(htmlOutput("sankey.plot"), height = 0, width = 0 
                  # box(plotOutput("plot1", height = 600)),
                  # box(
                  #   title = "Controls",
                  #   sliderInput("slider", "Number of observations:", 1, 100, 50)
                  )
                )
        ),
        
        # Second tab content
        tabItem(tabName = "globe",
                h2("Widgets tab content")
        )
      )
    )
  )
  

server <- function(input, output) {
 # sub_df <- reactive({df_sankey[df_sankey$Year == input$year, ]})

  sub_df_tree <- reactive({
    df_tree[df_tree$Year == input$year, ]
  })

  sub_df_sankey <- reactive({
    df_sankey_year <- df_sankey[df_sankey$Year == input$year, c('Reporter_description','Partner_description','Value')]
    if (input$radio.left == "Countries"){
      df_sankey_left <- df_sankey_year[! (df_sankey_year$Reporter_description %in% non_countries), ]
    } else {
      df_sankey_left <- df_sankey_year[(df_sankey_year$Reporter_description %in% non_countries), ]
    }

    if (input$radio.right == "Countries"){
      df_sankey_right <- df_sankey_left[! (df_sankey_left$Partner_description %in% non_countries_2), ]
    } else {
      df_sankey_right <- df_sankey_left[(df_sankey_left$Partner_description %in% non_countries_2), ]
    }
    return(df_sankey_right)
    })

   # output$Bubble.Plot <- renderPlotly({
   #   plot_ly(sub_df(), x = ~get(input$x), y = ~get(input$y), color = ~Region, size=~Population) %>%
   #   layout(xaxis =  list(title = input$x, showgrid = T), yaxis = list(title = input$y, showgrid = T))
   # })
   #

   # output$par_plot <- renderPlot({
   #   df_2 <- na.omit(sub_df()[, c("Region", "Country", "Population", "Employment", "Gross national savings-Percent of GDP-", "GDP, current prices-USD-Billions","Total investment-Percent of GDP-")])
   #   ggparcoord(data = df_2, columns = 3:ncol(df_2), scale = 'uniminmax', scaleSummary = "mean", splineFactor = F, groupColumn = "Region") +
   #     geom_dl(aes(label = Country), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
   #     theme_gdocs() + scale_color_gdocs(guide=FALSE) +
   #     theme(axis.title = element_blank())
   # })

  # output$google_chart <- reactive({# Filter to the desired year, and put the columns in the order that Google's Bubble Chart expects them (name, x, y, color, size). Also sort by region so that Google Charts orders and colors the regions consistently.
  # list(
  #   data = googleDataTable(sub_df()[, c('Country', input$x, input$y, 'Region', 'Population')]),
  #   options = list(
  #     title = paste0(input$x, " vs. ", input$y, " for year ", input$year),
  #     hAxis = list(title = input$x, viewWindow = xlim),
  #     vAxis = list(title = input$y, viewWindow = ylim)
  #       )
  #     )
  #   })

  # output$map <- renderGvis({
  #   # subset <- df.dcast[df.dcast$year == input$year,c("Country","Population")]
  #   map <- gvisGeoChart(sub_df(), locationvar="Country",
  #                  colorvar=input$x,
  #                  options=list(projection="kavrayskiy-vii", displayMode="regions",height = 600, width = 900))
  #   return(map)})

  # output$plot <- renderGvis({
  #   Sankey = gvisSankey(DataF,from="From", to="To", weight="Ponder",
  #                       options=list(width = "1200",
  #                                    height = "600",
  #                                    sankey="{
  #                                    link: {colorMode: 'gradient', color: { fill: '#green' } },
  #                                    node: {label: { color: 'black'},nodePadding: 80, width:50, color: { fill: '#a61d4c'} },
  # }"))
  #   })

  output$sankey.plot <- renderGvis({
    gvisSankey(sub_df_sankey(), from="Reporter_description", to="Partner_description", weight="Value"
                           , options=list(height = 600, width = 900)
              )
    })

  #
  # output$description <- renderText({
  #   nrow(sub_df_sankey()[, 1:2]) - nrow(unique(sub_df_sankey()[, 1:2]))
  # })

  # output$table <- renderDataTable(sub_df_sankey())

  output$d3tree <- renderD3tree2({
    m <- sub_df_tree()
    tm <- treemap(
      m[1:200, ],
      # m[m$year >= input$year.start, ],
      # index=c("Reporter_description", "Indicator_code"),
      index=c("Reporter_description", "Flow_Description", "Indicator_description"),
      vSize="Value",
      # vColor="Flow_Code",
      # type="Indicator_code",
      palette="-RdGy",
      # clickAction=paste0("Rating: ", ~rating),
      fontsize.legend = 8,
      format.legend = list(scientific = FALSE, big.mark = " "))
    d3tree2(tm, rootname = "International Trade" )
  })

}


# server <- function(input, output) { }
  
shinyApp(ui, server)

# 
#   
# 
# # Define UI for application that plot the data
# ui <- fluidPage(
#    titlePanel("Data Viz Project by Alice Zhao and Valentin Vrzheshch"),
#      mainPanel(
#        tabsetPanel(
#          # tabPanel("bubble plot", plotlyOutput("Bubble.Plot")),
#          # tabPanel("parallel coordinates plot", plotOutput("par_plot")),
#          # tabPanel("dataset", uiOutput("description")),
#          # tabPanel("Tables", fluidRow(dataTableOutput('table'))),
#          tabPanel("Countries to Regions Exports", 
#                   radioButtons("radio.left", "Left:", c("Countries" = "Countries", "Regions" = "Regions"), selected="Countries"),
#                   radioButtons("radio.right", "Right:", c("Countries" = "Countries", "Regions" = "Regions"), selected="Regions"),
#                   htmlOutput("sankey.plot")),
#          tabPanel("Tree Map of International Trade",
#                   # radioButtons("radio.indicator", "Indicator:", c(1,2)),
#                   radioButtons("radio.from", "From:", unique(df_tree$Reporter_description)),
#                   # radioButtons("radio.indicator", "Indicator:", unique(df_tree$Indicator_description)),
#                   # radioButtons("radio.flow", "Flow:", unique(df_tree$Flow_description)),
#                   d3tree2Output("d3tree"))
#          
#          # tabPanel("Google Chart",
#          #          # This line loads the Google Charts JS library
#          #          # googleChartsInit(chartTypes = 'sankey'), 
#          #          googleChartsInit(),
#          #          
#          #          #googleChartsInit(chartTypes ="geomap")
#          #          #   googleChartsInit(chartTypes = c("ALL", "annotatedtimeline", "area", "bar", "bubble", "calendar", "candlestick", "column", "combo", "gauge", "geo", "geomap", "intensitymap", "line", "map", "motion", "org", "pie", "sankey", "scatter", "steppedarea", "table", "timeline", "treemap"))
#          #          googleBubbleChart("google_chart", width = '900px', height='600px',
#          #                            options = list(fontName = "Comic Sans", fontSize = 11),  
#          #                            chartArea = list(
#          #                              top = 5, left = 7,
#          #                              height = "75%", width = "100%")
#          #          )),
#          
#          # tabPanel("google map", htmlOutput("map"))
#          ) 
#        ),
#      fluidRow(
#        shiny::column(4, offset = 4,
#                      sliderInput("year", "Year", min = min(df$Year), max = max(df$Year),
#                                  value = max(df_sankey$Year)-5, animate = TRUE, step = 1, sep = "")
#            # selectInput("x", label = h3("Select x-axis:"), choices = unique(df$Reporter_description)), #choices = unique(df$Subject)
#            # selectInput("y", label = h3("Select y-axis:"), choices = unique(df$Partner_description))#, selected = "Employment")
#        )
#      )
# )
#       # sidebarPanel(
#      # wellPanel(
#      #     sliderInput("year",
#      #                 h3("Choose year:"),
#      #                 min = min(df.dcast$year, na.rm = T),
#      #                 max = max(df.dcast$year, na.rm = T),
#      #                 value = 2005, step=1, animate = T),
#      #     selectInput("x", label = h3("Select x-axis:"), choices = unique(df$Subject.Descriptor), selected = 'Population'),
#      #     selectInput("y", label = h3("Select y-axis:"), choices = unique(df$Subject.Descriptor), selected = "Employment")
#      #  )
# 
# 
# # Define server logic required to plot the dataset
# server <- function(input, output) {
#  # sub_df <- reactive({df_sankey[df_sankey$Year == input$year, ]})
#   
#   sub_df_tree <- reactive({
#     df_tree[df_tree$Year == input$year, ]
#   })
#   
#   sub_df_sankey <- reactive({
#     df_sankey_year <- df_sankey[df_sankey$Year == input$year, c('Reporter_description','Partner_description','Value')]
#     if (input$radio.left == "Countries"){
#       df_sankey_left <- df_sankey_year[! (df_sankey_year$Reporter_description %in% non_countries), ]
#     } else {
#       df_sankey_left <- df_sankey_year[(df_sankey_year$Reporter_description %in% non_countries), ]      
#     }
#     
#     if (input$radio.right == "Countries"){
#       df_sankey_right <- df_sankey_left[! (df_sankey_left$Partner_description %in% non_countries_2), ]
#     } else {
#       df_sankey_right <- df_sankey_left[(df_sankey_left$Partner_description %in% non_countries_2), ]      
#     }
#     return(df_sankey_right)
#     })
#    
#    # output$Bubble.Plot <- renderPlotly({
#    #   plot_ly(sub_df(), x = ~get(input$x), y = ~get(input$y), color = ~Region, size=~Population) %>%
#    #   layout(xaxis =  list(title = input$x, showgrid = T), yaxis = list(title = input$y, showgrid = T)) 
#    # })
#    # 
#   
#    # output$par_plot <- renderPlot({
#    #   df_2 <- na.omit(sub_df()[, c("Region", "Country", "Population", "Employment", "Gross national savings-Percent of GDP-", "GDP, current prices-USD-Billions","Total investment-Percent of GDP-")])
#    #   ggparcoord(data = df_2, columns = 3:ncol(df_2), scale = 'uniminmax', scaleSummary = "mean", splineFactor = F, groupColumn = "Region") +
#    #     geom_dl(aes(label = Country), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +  
#    #     theme_gdocs() + scale_color_gdocs(guide=FALSE) + 
#    #     theme(axis.title = element_blank())
#    # })
#   
#   # output$google_chart <- reactive({# Filter to the desired year, and put the columns in the order that Google's Bubble Chart expects them (name, x, y, color, size). Also sort by region so that Google Charts orders and colors the regions consistently.
#   # list(
#   #   data = googleDataTable(sub_df()[, c('Country', input$x, input$y, 'Region', 'Population')]),
#   #   options = list(
#   #     title = paste0(input$x, " vs. ", input$y, " for year ", input$year),
#   #     hAxis = list(title = input$x, viewWindow = xlim),
#   #     vAxis = list(title = input$y, viewWindow = ylim)
#   #       )
#   #     )
#   #   })
#   
#   # output$map <- renderGvis({
#   #   # subset <- df.dcast[df.dcast$year == input$year,c("Country","Population")]
#   #   map <- gvisGeoChart(sub_df(), locationvar="Country",
#   #                  colorvar=input$x,
#   #                  options=list(projection="kavrayskiy-vii", displayMode="regions",height = 600, width = 900))
#   #   return(map)})
#   
#   # output$plot <- renderGvis({
#   #   Sankey = gvisSankey(DataF,from="From", to="To", weight="Ponder",
#   #                       options=list(width = "1200",
#   #                                    height = "600",
#   #                                    sankey="{
#   #                                    link: {colorMode: 'gradient', color: { fill: '#green' } },
#   #                                    node: {label: { color: 'black'},nodePadding: 80, width:50, color: { fill: '#a61d4c'} },
#   # }"))
#   #   })
#   
#   output$sankey.plot <- renderGvis({
#     gvisSankey(sub_df_sankey(), from="Reporter_description", to="Partner_description", weight="Value"
#                            , options=list(height = 600, width = 900)
#               )
#     })
# 
#   # 
#   # output$description <- renderText({
#   #   nrow(sub_df_sankey()[, 1:2]) - nrow(unique(sub_df_sankey()[, 1:2]))
#   # })
#   
#   # output$table <- renderDataTable(sub_df_sankey())
#   
#   output$d3tree <- renderD3tree2({
#     m <- sub_df_tree()
#     tm <- treemap(
#       m[1:200, ],
#       # m[m$year >= input$year.start, ],
#       # index=c("Reporter_description", "Indicator_code"),
#       index=c("Reporter_description", "Flow_Description", "Indicator_description"),
#       vSize="Value",
#       # vColor="Flow_Code",
#       # type="Indicator_code",
#       palette="-RdGy",
#       # clickAction=paste0("Rating: ", ~rating),
#       fontsize.legend = 8,
#       format.legend = list(scientific = FALSE, big.mark = " "))
#     d3tree2(tm, rootname = "International Trade" )
#   })
#   
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
# # 
# # 
# # # china <- df.dcast[df.dcast$Country == 'China',]
# # # 
# # # subset <- df.dcast[df.dcast$year == 2003,c("Country","Population")]
# # # Geo=gvisGeoChart(subset, locationvar="Country", 
# # #                  colorvar="Population",options=list(projection="kavrayskiy-vii",displayMode="regions"))
# # # plot(Geo)
# # # 
# # # Run the application 
# # shinyApp(ui = ui, server = server)
# # 
# # # Data source: http://goo.gl/vcKo6y
# # UKvisits <- data.frame(origin=c(
# #   "France", "Germany", "USA",
# #   "Irish Republic", "Netherlands",
# #   "Spain", "Italy", "Poland",
# #   "Belgium", "Australia", 
# #   "Other countries", rep("UK", 5)),
# #   visit=c(
# #     rep("UK", 11), "Scotland",
# #     "Wales", "Northern Ireland", 
# #     "England", "London"),
# #   weights=c(
# #     c(12,10,9,8,6,6,5,4,4,3,33)/100*31.8, 
# #     c(2.2,0.9,0.4,12.8,15.5)))
# # 
# # 
# # 
# # 
# # plot(
# #   gvisSankey(UKvisits, from="origin", 
# #              to="visit", weight="weight",
# #              options=list(
# #                height=250,
# #                sankey="{link:{color:{fill:'lightblue'}}}"
# #              ))
# # )
# # 
# # require(igraph)
# # require(googleVis)
# # g <- graph.tree(24, children = 4)
# # set.seed(123)
# # E(g)$weight = rpois(23, 4) + 1
# # edgelist <- get.data.frame(g) 
# # colnames(edgelist) <- c("source","target","value")
# # edgelist$source <- LETTERS[edgelist$source]
# # edgelist$target <- LETTERS[edgelist$target]
# # 
# # plot(
# #   gvisSankey(edgelist, from="source", 
# #              to="target", weight="value",
# #              options=list(
# #                sankey="{link: {color: { fill: '#d799ae' } },
# #                node: { width: 4, 
# #                color: { fill: '#a61d4c' },
# #                label: { fontName: 'Times-Roman',
# #                fontSize: 14,
# #                color: '#871b47',
# #                bold: true,
# #                italic: true } }}"))
# #              )
# # 
# # 
# # #Sankey plot example from our data
# # 
# # # data2 <- data[!(data$Reporter_code %in% unique(data$Partner_code)) & data$Year == '2010', c('Reporter_code','Partner_code','Value')]
# # # data2 <- data[!(data$Partner_description %in% unique(data$Reporter_description)) & data$Year == '2010' & data$Partner_description != 'World', c('Reporter_description','Partner_description','Value')]
# # # data2 <- data[!(data$Report_description %in% unique(data$Partner_description)) & data$Year == '2010' & data$Partner_description != 'World', c('Reporter_description','Partner_description','Value')]
# # 
# # 
# # 
# # 
# # data <- data[-7,c('Reporter_code','Partner_code','Value')]
# # 
# # sk2 <- gvisSankey(data, from="Reporter_code", to="Partner_code", weight="Value",
# #                   options=list(sankey="{link: {color: { fill: '#d799ae' } },
# #                                node: { color: { fill: '#a61d4c' },
# #                                label: { color: '#871b47' } }}"))
# # plot(sk2)
# # 
# # typeof(data$Reporter_code)
# # 
# # typeof(data$Partner_code)
# # 
# # typeof(data$Value)
