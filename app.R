## Consumer Complaints ##
# Shiny dashboard example
# Data source: https://catalog.data.gov/dataset/consumer-complaint-database
# Michal Staniszewski

#============= LIBRARIES AND DATA ================

libs <- c("tidyr", "dplyr", "ggplot2", "lubridate")
lapply(libs, require, character.only = TRUE)

library(shiny)
library(shinydashboard)
library(leaflet)
library(lubridate)
library(plotly)
library(formattable)
library(d3heatmap)

# Loading data
comp <- read.csv("data/comp.csv")

# Adding coordinates for postal codes from www.geonames.org
zipy <- read.table("data/US.txt", sep="\t", encoding = "utf-8", fill = TRUE)
zipy <- select(zipy, V2, V10, V11)
colnames(zipy) <- c("ZIP.code", "lat", "long")
zipy$ZIP.code <- as.factor(zipy$ZIP.code)
comp <- left_join(comp, zipy, by = "ZIP.code")

#============= PREPROCESSING ====================

#Dates formating
comp$Date.sent.to.company <-mdy(comp$Date.sent.to.company)
comp$Date.received <- mdy(comp$Date.received)

# Leaflet data
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
na_mape <- select(comp, lat, long, Company, Product)
na_mape <- filter(na_mape, !is.na(lat))

# Choices for dropdown and dates
product_choices <- as.list(data.frame(table(comp$Product))$Var1)
start_date <- min(comp$Date.received)
end_date <- max(comp$Date.received)

# Summary table - response rate
by_response <- group_by(comp, Timely.response.)
response <- summarise(by_response, suma = n())

by_disputed <- group_by(comp, Consumer.disputed.)
disputed_all <- summarise(by_disputed, suma = n())

### Summary table - top30

# Count frequencies
top30 <- data.frame(table(comp$Company))
top30 <- arrange(top30, desc(Freq))
colnames(top30) <- c("Company", "num")
top30 <- mutate(top30, percent_comp = round((num/sum(top30$num)*100), 1))

# Count late response rates
by_response_comp <- group_by(comp, Company, Timely.response.)
response_comp <- summarise(by_response_comp,
                           suma = n()
)
response_comp <- spread(response_comp, Timely.response., suma)
response_comp <- arrange(response_comp, desc(Yes))

# Count disputed cases
disputed <- group_by(comp, Company, Consumer.disputed.)
disputed <- summarise(disputed,
                      suma = n()
)
disputed <- spread(disputed, Consumer.disputed., suma)
disputed <- arrange(disputed, desc(Yes))
disputed <- mutate(disputed, dispute_percent = round((Yes/(Yes+No))*100, 2))

# Merge and clean final list for dashboard display
top30 <- left_join(top30, response_comp, by = "Company")
top30 <- left_join(top30, disputed, by = "Company")
top30 <- select(top30, Company, num, percent_comp, No.x, Yes.y, dispute_percent)
top30 <- head(top30, 30)
colnames(top30) <- c("Company Name", "Num of complaints", "% of complaints", "Num of late responses", "Number of disputed cases", "% of disputed cases")
top30[is.na(top30)] <- 0

# State Structure
prod_states <- data.frame(table(comp$Product, comp$State))
colnames(prod_states) <- c("Product", "State", "Freq")
prod_states <- spread(prod_states, State, Freq)
rownames <- prod_states$Product
prod_states <- sapply(prod_states[,-1], as.numeric)
row.names(prod_states) <- rownames
prod_states <- prod_states[,order(colSums(-prod_states, na.rm=TRUE))]

# Issues
issues <- data.frame(table(comp$Product, comp$Issue))
issues <- filter(issues, Freq > 0)
colnames(issues) <- c("Product", "Issue", "Freq")

# Time series
received <- data.frame(table(comp$Date.received, comp$Product))
colnames(received) <- c("Date", "Product", "Freq")
received$Date <- ymd(received$Date)
received <- filter(received, Date < "2014-02-01" & Date > "2013-07-01")

#============= CLIENT UI ===========================

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Consumer Complaints"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("summary", tabName = "summary"),
      menuItem("analysis", tabName = "analysis"),
      menuItem("map", tabName = "map")
    )),
  
  dashboardBody(
    # Custom styles
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
    
# Tab 1 - summary  -----------------
      
      tabItem("summary",
              fluidRow(
                valueBoxOutput("totalBox"),
                valueBoxOutput("disputeBox"),
                valueBoxOutput("lateBox")
              ),
              fluidRow(
                box(width = "100%",
                    title = "Summary statistics for top 30 companies considering number of customers complaints:",
                    formattableOutput("table")
  
                    )
              )
            ),
      
      
# Tab 2 - analysis  -----------------
      
      tabItem("analysis",
              fluidRow(
                
                box(
                  title = "Time trends by product type",
                  selectInput("select_product", label = "Product type", 
                              choices = product_choices, 
                              selected = 1),
                  plotOutput("plot2", height = 300)
                ),
                box(
                  title = "Issues raised by customers",
                  plotlyOutput("plot3", height = 375)
                )

              ),
              
              fluidRow(
                box(
                  title = "Complaints by product type and state",
                  width = "100%",
                  checkboxInput("cluster", "Apply clustering"),
                  d3heatmapOutput("heatmap")
                )
              )
      ),
      

# Tab3 - map  -----------------
    
    tabItem("map",
            div(class="outer", leafletOutput("mymap",   width="100%", height="100%"))
    )
    
  )))

#============= SERVER ==========================


server <- function(input, output) {

# Tab 1 - summary -----------------

  num_complaints <- nrow(comp)
  output$totalBox <- renderValueBox({
    valueBox(
      paste0(num_complaints), "Num of complaints", icon = icon("users"),
      color = "blue"
    )
  })
  
  
  disputed_stat <- round((disputed_all[3,2]/disputed_all[2,2])*100, 2)
  output$disputeBox <- renderValueBox({
    valueBox(
      paste0(disputed_stat, "%"), "Dsiputed cases", icon = icon("bullhorn"),
      color = "yellow"
    )
  })

  late_stat <- round((response[1,2]/response[2,2])*100, 2)
  output$lateBox <- renderValueBox({
    valueBox(
      paste0(late_stat, "%"), "Late response", icon = icon("calendar-times-o"),
      color = "red"
    )
  })
  
  output$table <- renderFormattable({
    
    formattable(top30, list(
      `% of complaints` = color_tile("white", "orange"),
      `Num of late responses` = formatter("span", style = x ~ ifelse(x == "A", style(color = "green", font.weight = "bold"), NA)),
      area(col = c(`Number of disputed cases`)) ~ normalize_bar("pink", 0.2),
      area(col = c(`% of disputed cases`)) ~ normalize_bar("pink", 0.2)
    ))
    
    })
  
  
# Tab 2 - analysis -----------------

  # Time series
  output$plot2 <- renderPlot({
    data2 <- filter(received, Product == input$select_product)
    p <- ggplot(data2, aes(Date, Freq))
    p + geom_point() + stat_smooth()
  })

  # Issues
  output$plot3 <- renderPlotly({
    data3 <- filter(issues, Product == input$select_product)
    data3$Issue <- factor(data3$Issue)
    data3 <- arrange(data3, desc(Freq))
    plot_ly(data = data3, x = data3$Freq, y = data3$Issue, type = 'bar', orientation = 'h') %>%
      layout(margin = list(l = 250), yaxis = list(size = 4))
  })
  
  # Heatmap
  output$heatmap <- renderD3heatmap({
    d3heatmap(prod_states,
      scale = "column",
      colors = "Spectral",
      xaxis_font_size = "10px",
      yaxis_font_size = "10px",
      dendrogram = if (input$cluster) "both" else "none"
    )
  })
  
# Tab3 - map -----------------
  
  output$mymap <- renderLeaflet({
    leaflet(data = na_mape) %>%
      addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~long, ~lat, popup = ~as.character(paste(Company, Product, sep=" - ")), clusterOptions = markerClusterOptions()) %>%
      setView(lng = -96, lat = 38 , zoom = 4)
  })
}


shinyApp(ui, server)
