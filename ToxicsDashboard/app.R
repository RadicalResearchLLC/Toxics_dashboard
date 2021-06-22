#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(htmltools)
library(gghighlight)

##### Header elements ####
ui <- fluidPage(title = 'Air Toxics Data Dashboard',
    tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
    
    titlePanel(fluidRow(
      column(9, "Air Toxics Data Dashboard"),
      column(3, shiny::img(height = 75, src = 'Logo.png')))
    ),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
#### Trends tab ####
      tabPanel("Trends",
        fluidRow(
          column(4, align = 'center', selectInput(inputId = 'pollutant', label = 'Select a pollutant',
            choices = sort(carcinogens$parameter), selected = '1,4-Dichlorobenzene')),
          column(5, align = 'center', selectizeInput(inputId = 'method', label = 'Select up to three method codes - or none for all',
            choices = '', selected = NULL, options = list(maxItems = 3)
            ))
        ),
        fluidRow(
          column(8, align = 'center', plotOutput("Trends", height = '600px')),
          column(4, align = 'center', leafletOutput("TrendSiteMap", height = '550px'))
        )
      ),
#### National comparison Tab ####
      tabPanel("National Site Comparisons",
        fluidRow(
          column(4, align = 'center', selectizeInput(inputId = 'RiskParam', label = 'Select up to 5 carcinogens',
            choices = sort(carcinogens$parameter), options = list(maxItems = 5))
            ),
          column(4, align = 'center', selectInput(inputId = 'Yr', label = 'Select a year',
           selected = '2019', choices = sort(YrList$yr))
           )
        ),
        fluidRow(
          column(8, align = 'center', plotOutput("RiskBar", height = '700px')),
          column(4, align = 'center', leafletOutput("RiskSiteMap"))
        )
      ),
#### Site Risk Context Tab ####
      tabPanel("Site Risk in Context",
        fluidRow(
          column(4, align = 'center', selectInput(inputId = 'Yr2', label = 'Select a year',
                                                  selected = '2019', choices = sort(YrList$yr)))
        ),
        fluidRow(
          column(5, align = 'center', leafletOutput("RiskContextMap")),
          column(6, align = 'center', dataTableOutput('ContextDT'))
        ),
        fluidRow(column(7, align = 'center', plotOutput("compareRisk", height = '600px')),
                 column(2, textOutput('siteInfo')))
        ),

#### Pollutant risk ranking tab ####
      tabPanel("Pollutant Risk Ranking",
        fluidRow(
         column(4, align = 'center', selectInput(inputId = 'Yr3', label = 'Select a year',
           selected = '2019', choices = sort(YrList$yr)))
               ),
        fluidRow(
          column(7, align = 'center', plotOutput("PollutantRisk", height = '700px')),
          column(5, align = 'center', dataTableOutput('RiskDT'))
               )
      )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
#### Reactives for Trends Tab ####  
  paramMethods <- reactive({
    methodCodeDF <- trend_count %>%
      filter(parameter == input$pollutant) %>%
      select(method_code) %>%
      unique() %>%
      arrange(method_code)
    
    return(methodCodeDF)
  })
  
  observe({ 
    updateSelectizeInput(session, 'method', choices = paramMethods()$method_code, selected = NULL)
  })

  trend_param <- reactive({
    if(is.null(input$method)){
    trend_data %>%
      filter(parameter == input$pollutant) }
    else({
    trend_data %>%
      filter(parameter == input$pollutant) %>%
      filter(method_code %in% input$method)
    })
  })
  
  trendSites <- reactive({
    trendSiteList <- trend_param() %>%
      select(aqs_sitecode) %>%
      distinct() %>%
      left_join(site_list2, by = c('aqs_sitecode'))
  })  

#### Trends figures ####          
  output$Trends <- renderPlot({
    trend_plot <- ggplot(data = trend_param(), aes(x = yr, y = cancerRisk, color = aqs_sitecode)) +
      geom_line(alpha = 0.8, size = 1.5) +
      gghighlight(max(cancerRisk), max_highlight = 5L) +
      geom_point(aes(shape = MDL_type), size = 3.5) +
      theme_bw() +
      geom_hline(color = 'orange', yintercept = 1, linetype = 'twodash') +
      geom_hline(color = 'red', yintercept = 100, linetype = 'twodash') +
      scale_y_log10() +
      labs(y = 'Cancer risk (per million)',
        x = '') +
      scale_x_continuous(limits = c(2000,2020), breaks = c(2000, 2004, 2008, 2012, 2016, 2020)) +
      scale_shape_manual(name = 'Percent below MDL', values = c('<50% below MDL' = 15, '50-75% below MDL' = 16, 
        '75-90% below MDL' = 5, '>90% below MDL' = 6)) +
      theme(text = element_text(size = 18), legend.position = 'bottom')
      
      trend_plot
    })
  
  output$TrendSiteMap <- renderLeaflet({

    map2 <- leaflet(data = trendSites()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ longitude,
                       lat = ~ latitude,
                       label = ~htmlEscape(local_site_name),
                       radius = 6, stroke = FALSE, fillOpacity = 0.5) %>%
      addProviderTiles("Esri.WorldImagery", group = 'Imagery') %>%
      addLayersControl(baseGroups = c('Basemap', 'Imagery'))
    
    map2
  })
  
#### Reactives for National Comparisons ####  
   
  RiskSites <- reactive({
    
    countParam <- length(input$RiskParam)
    
    annual_risk %>%
      filter(yr == input$Yr) %>%
      filter(parameter %in% input$RiskParam) %>%
      group_by(aqs_sitecode, parameter, yr) %>%
      summarize(countRisk = n(), .groups = 'drop') %>%
      group_by(aqs_sitecode, yr) %>%
      summarize(countParams = n(), .groups = 'drop') %>%
      filter(countParams == countParam) %>%
        left_join(site_list2, by = c('aqs_sitecode'))
    
    
    })
  
  RiskValues <- reactive({
    risky <- annual_risk %>%
      right_join(RiskSites(), by = c('aqs_sitecode', 'yr')) %>%
      filter(parameter %in% input$RiskParam)
    
    return(risky)
  })

#### outputs for National Site Comparisons ####    
  output$RiskSiteMap <- renderLeaflet({
    
    map3 <- leaflet(data = RiskSites()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ longitude,
                       lat = ~ latitude,
                       label = ~htmlEscape(aqs_sitecode),
                       radius = 6, stroke = FALSE, fillOpacity = 0.5) %>%
      addProviderTiles("Esri.WorldImagery", group = 'Imagery') %>%
      addLayersControl(baseGroups = c('Basemap', 'Imagery'))
    
    map3
  })
  
  output$RiskBar <- renderPlot({
    RiskBars <- ggplot( data = RiskValues(), aes(x = aqs_sitecode, y = cancerRisk,
      fill = parameter)) +
      geom_col(position = 'stack') +
      theme_bw() +
      #geom_hline(color = 'orange', yintercept = 1, linetype = 'twodash') +
      geom_hline(color = 'red', yintercept = 100, linetype = 'twodash') +
     #scale_y_log10() +
      labs(y = 'Summed Cancer risk (per million)',
           x = 'Site Code') +
      theme(text = element_text(size = 18), legend.position = 'bottom') +
      coord_flip()
    
    RiskBars
  }) 
  
#### Site Risk Reactives ####  
  
  Risk4Context <- reactive({
    risky2 <- annual_risk %>%
      filter(yr == input$Yr2)
    
    return(risky2)
  })
  
  ContextSites <- reactive({
    yrSites <- Risk4Context() %>%
      left_join(site_list) %>%
      select(aqs_sitecode, latitude, longitude, local_site_name) %>%
      distinct() %>%
      rename('site name' = 'local_site_name')
  })  
  
  SelectedSite <- reactive({
    req(input$RiskContextMap_marker_click) 
    
    Risk4Context() %>%
      filter(aqs_sitecode == as.character(input$RiskContextMap_marker_click[1])) %>%
      filter(cancerRisk > 0)
  }) 
  
  Risk4Context2 <- reactive({
    #req(SelectedSite())
    
    SelectedSite() %>% 
      select(parameter) %>%
      distinct() %>%
      left_join(annual_risk) %>%
      filter(yr == input$Yr2) %>%
      mutate(parameter2 = as.factor(parameter))
  })  
  
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 20%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))
  
  title <- tags$div(
    tag.map.title, HTML("Left-click a site on the map")
  )  

#### Site Risk Figures ####  
    
  output$RiskContextMap <- renderLeaflet({
    
    map4 <- leaflet(data = ContextSites()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ longitude,
                       lat = ~ latitude,
                       label = ~htmlEscape(aqs_sitecode),
                       layerId = ~aqs_sitecode,
                       radius = 6, stroke = FALSE, fillOpacity = 0.5) %>%
     addProviderTiles("Esri.WorldImagery", group = 'Imagery') %>%
     addLayersControl(baseGroups = c('Basemap', 'Imagery')) %>%
     addControl(title, position = "topleft", className="map-title")
    
    map4
  })
  
  output$ContextDT <- renderDataTable(
    ContextSites(),
      #caption  = 'Table 3: Project tasks, hours, and costs',
      #rownames = FALSE, 
      options = list(dom = 'tp',
       pageLength = 10) #%>%
      #formatStyle('Task Name',
      #            target = 'row',
      #           backgroundColor = styleEqual(rowNames,
      #                                         c('light gray', 'white', 'light gray', 'white', '#cccccc')),
      #            fontWeight = styleEqual(rowNames,
      #                                    c('normal', 'normal', 'normal', 'normal', 'bold'))
     )
 
# output$siteInfo <- renderText({
#  req(input$RiskContextMap_marker_click) 
#  
#  return(SelectedSite()$aqs_sitecode) 
# })
 


 #output$siteInfo <- renderText({
#  req(input$RiskContextMap_marker_click) 
#  
#  return(Risk4Context2()$cancerRisk) 
# })

output$compareRisk <- renderPlot({
  
  boxRisk <- ggplot(data = Risk4Context2(), aes(x = parameter, y = cancerRisk)) +
   geom_boxplot() + 
   geom_jitter(shape = 1, color = 'gray', alpha = 0.7) +
   theme_bw() +
    geom_hline(color = 'orange', yintercept = 1, linetype = 'twodash') +
    geom_hline(color = 'red', yintercept = 100, linetype = 'twodash') +
    scale_y_log10() +
    labs(title = paste('Risk comparison for selected site', 
      as.character(input$RiskContextMap_marker_click[1]), 'for the year',
      as.character(input$Yr2)),
      y = 'Cancer risk (per million)',
      x = '') +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 50, hjust = 1)) +
    geom_point(data = SelectedSite(), aes(x = parameter, y = cancerRisk), shape = 17,
               size = 4, color = 'blue')
 
 boxRisk
   
  })

#### Pollutant Risk reactive and Figure ####

pollutantRisk <- reactive({
  statDF <- annualRiskStats %>%
    filter(yr == input$Yr3) %>%
    filter(countAbove1 > 3) %>%
    arrange(desc(medRisk)) %>%
    left_join(annual_risk) %>%
    mutate(MDL.Category2 = as.factor(case_when(
      pctBelowMDL < 50 ~ '<50% below MDL on avg',
      pctBelowMDL < 75 ~ 'Between 50 and 75% below MDL on avg',
      pctBelowMDL < 90 ~ 'Between 75 and 90% below MDL on avg',
      pctBelowMDL <= 100 ~'More than 90% below MDL on avg'
    )))
})

riskStatsDT <- reactive({
  statsDT <- annualRiskStats %>%
    filter(yr == input$Yr3) %>%
    filter(countAbove1 > 3) %>%
    arrange(desc(medRisk)) %>%
    select(-MDL.Category) %>%
    mutate('Mean cancer risk' = round(avgRisk, 1), 'Max cancer risk' = round(maxRisk,1), 
           'Median cancer risk' = round(medRisk, 1),
           "% Below MDL" = round(avgPctBelowMDL, 1)) %>%
    rename(Year = yr, 'Site count' = countSites,
           'Site count with cancer risk above 1' = countAbove1,
           'Site count with cancer risk above 100' = countAbove100) %>% 
    select(-avgRisk, -medRisk, -maxRisk, -avgPctBelowMDL)
})

output$PollutantRisk <- renderPlot({
  
  ggplot(data = pollutantRisk(), aes(x = reorder(parameter, desc(medRisk)), y = cancerRisk, fill = MDL.Category)) +
    theme_bw() +
    geom_hline(color = 'orange', yintercept = 1, linetype = 'twodash') +
    geom_hline(color = 'red', yintercept = 100, linetype = 'twodash') +
    geom_boxplot(alpha = 0.5) + 
    geom_jitter(shape = 1,  alpha = 0.6, aes(color = MDL.Category2)) +
    scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000), limits = c(0.008, 3500)) +
    labs(title = paste('Pollutant cancer risk ranking for the year',
                       as.character(input$Yr3), '- gray numbers indicate site counts'),
         y = 'Cancer risk (per million)',
         x = '') +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 50, hjust = 1),
          legend.position = 'bottom') +
    scale_fill_manual(name = 'Percent below MDL', values = c('<50% below MDL on avg' = 'gray',
      'Between 50 and 75% below MDL on avg' = 'yellow', 
      'Between 75 and 90% below MDL on avg' = 'orange', 
      'More than 90% below MDL on avg' = 'red')) +
    scale_color_manual(name = 'Percent below MDL', values = c('<50% below MDL on avg' = 'dark gray',
     'Between 50 and 75% below MDL on avg' = 'yellow', 
     'Between 75 and 90% below MDL on avg' = 'orange', 
     'More than 90% below MDL on avg' = 'red')) +
    geom_text(data = pollutantRisk(), aes(x = reorder(parameter, desc(medRisk)), y = maxRisk,
     label = countSites), color = 'gray', nudge_y = 0.2, size = 6) 
    
})

output$RiskDT <- renderDataTable(
  riskStatsDT(),
  #caption  = 'Table 3: Project tasks, hours, and costs',
  #rownames = FALSE, 
  options = list(dom = 'tp',
                 pageLength = 10) #%>%
  #formatStyle('Task Name',
  #            target = 'row',
  #           backgroundColor = styleEqual(rowNames,
  #                                         c('light gray', 'white', 'light gray', 'white', '#cccccc')),
  #            fontWeight = styleEqual(rowNames,
  #                                    c('normal', 'normal', 'normal', 'normal', 'bold'))
)


   
}

# Run the application 
shinyApp(ui = ui, server = server)
