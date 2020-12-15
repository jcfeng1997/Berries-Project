library(shiny)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(ggplot2)

ag_data <- read_csv("berries.csv", col_names = TRUE)

## look at number of unique values in each column
ag_data %>% summarize_all(n_distinct) -> aa


## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique valu column names 
colnames(ag_data)[bb]

## list the 1-unique single values.  
## Consider if they should be used for labels

single_values <- ag_data[1,bb]


## remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))

## look at number of unique values in each column
ag_data %>% summarize_all(n_distinct) -> aa


## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique valu column names 
colnames(ag_data)[bb]

## list the 1-unique single values.  
## Consider if they should be used for labels

single_values <- ag_data[1,bb]


## remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))

## Make a table of the number of unique values in each column.
aa %<>% select(-all_of(bb)) 

## State name and the State ANSI code are (sort of) redundant


ag_data %<>% select(-4)
aa %<>% select(-4) 


ag_data$Year %>%  unique()
## [1] 2019 2018 2017 2016 2015

ag_data$Period %>% unique()

## blueberry data
ag_data_bb <- ag_data %>% filter((Commodity=="BLUEBERRIES") & (Period=="YEAR"))

ag_data_bb %<>% separate(`Data Item`, c("berry", "type", "data_item", "unit"), ",")

ag_data_bb %<>% select(-c(Period,Commodity,berry))

kable(head(ag_data_bb)) %>% kable_styling(font_size=12)

#######################################################

### Then focus on: period = "Year" and Commodity = "strawberries"

## Strawberry data
ag_data_sb <- ag_data %>% filter((Commodity=="STRAWBERRIES") & (Period=="YEAR"))

ag_data_sb %<>% separate(`Data Item`, c("berry", "type", "data_item", "unit"), ",")

ag_data_sb %<>% select(-c(Period,Commodity,berry))

kable(head(ag_data_sb)) %>% kable_styling(font_size=12)

#######################################################

### Also,period = "Year" and Commodity = "raspberries"

## Raspberry data
ag_data_rb <- ag_data %>% filter((Commodity=="RASPBERRIES") & (Period=="YEAR"))

ag_data_rb %<>% separate(`Data Item`, c("berry", "type", "data_item", "unit"), ",")

ag_data_rb %<>% select(-c(Period,Commodity,berry))

kable(head(ag_data_rb)) %>% kable_styling(font_size=12)


appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  title = "Berry project",
  
  div(id = "header",
      h1("Berry project"),
      strong( 
        span("Created by "),
        a("Jiachen Feng", href = "https://github.com/jcfeng1997"),
        HTML("&bull;"),
        span("Details on"),
        a("GitHub", href = "https://github.com/jcfeng1997/Berries-Project"),
        HTML("&bull;"))
  ),
  
  sidebarLayout(
    sidebarPanel(
       strong("Data cleaning and EDA using Berries data") ,
      
      br(),          
      radioButtons("typeInput", "Berry types",
                         choices = c("Blueberry" = "blueberry", 
                                     "Strawberry" = "strawberry", 
                                     "Raspberry" = "raspberry"),
                         selected = 1,
      ) 
                 
    ),
    
    mainPanel(
      tabsetPanel( 
        tabPanel("EDA",
                 strong("Planting methods in different states"),
                 br(),
                 plotOutput("plot") ),
                 
        tabPanel("Data Table",
                 br(),
                 tableOutput("data")
        )
        
       
       
        
      )
    )
  )
)

# Sever
server <- function(input, output, session) {
 
  filterdata <- reactive({
    if (input$typeInput=="blueberry"){
      ag_data_bb
    } else if(input$typeInput=="strawberry")
      {
      ag_data_sb
    }
    else{
      ag_data_rb}
  })
  
  filtereda <- reactive({
    if (input$typeInput=="blueberry"){
      bbplot1 <- ggplot(ag_data_bb, aes(x = State, y = Domain))
      bbplot1 <- bbplot1 + geom_point() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              axis.text = element_text(size = 11),
              axis.title = element_text(size = 13, face = "bold")) +
        labs(x = "State",y="Domain",title = "Blueberries")+
        theme(plot.title = element_text(hjust = 0.5))
      bbplot1
    } else if(input$typeInput=="strawberry")
    {
      rbplot1 <- ggplot(ag_data_rb, aes(x = State, y = Domain))
      rbplot1 <- rbplot1 + geom_point() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              axis.text = element_text(size = 11),
              axis.title = element_text(size = 13, face = "bold")) +
        labs(x = "State",y="Domain",title = "Raspberries")+
        theme(plot.title = element_text(hjust = 0.5))
      rbplot1
    }
    else{
      sbplot1 <- ggplot(ag_data_bb, aes(x = State, y = Domain))
      sbplot1 <- sbplot1 + geom_point() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              axis.text = element_text(size = 11),
              axis.title = element_text(size = 13, face = "bold")) +
        labs(x = "State",y="Domain",title = "Strawberries")+
        theme(plot.title = element_text(hjust = 0.5))
      sbplot1
      }
  })
  output$data <- renderTable({
    filterdata()
  })
  
  output$plot <- renderPlot({
    filtereda()
  })
  
  
}

# Run
shinyApp(ui = ui, server = server)
