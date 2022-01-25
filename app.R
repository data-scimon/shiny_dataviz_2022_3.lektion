library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(DT)

library(shinyWidgets)

library(googlesheets4)

gs4_auth(cache = ".secrets",
         email = "simonmj91@hotmail.com")




passat <- read_excel("passat.xlsx")
rt <- read_csv2("Rt_cases.csv")

navne <- read_excel("navn_test.xlsx")



theme_set(theme_minimal())
options(scipen = 999)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Datavisualisering 2022", title = div(img(height = 100,
                                                         width = 125,
                                                         src = "dania_logo.png"))),
    theme = shinytheme("cosmo"),
    
    headerPanel("Dette er vores web-app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            
            
            sliderInput("alpha", "Vælg transparenthed",
                        min = 0.1,
                        max = 1.0,
                        value = 0.5),
            
            sliderInput("size", "Vælg størrelse på punkter",
                        min = 1,
                        max = 20,
                        value = 5),
            
            sliderInput("price_adj", "Vælg max pris",
                        min = min(passat$price),
                        max = max(passat$price),
                        value = median(passat$price),
                        step = 10000),
            
            #textInput("title", "Skriv din titel her"),
            
            
            
            # submitButton("Lav ændringer"),
            
            hr(),
            
            helpText("Dataopsamling"),
            
           textInput("bruger", "Skriv dit navn"),
            
            dateInput("date", "Fødselsdato", 
                      value = Sys.Date(), 
                      min = NULL,
                      max = NULL,
                      format = "yyyy-mm-dd",
                      startview = "year",
                      weekstart = 1,
                      width = NULL),
                           
            textInput("fødeby", "Fødeby",
                      placeholder = "Vælg din fødeby"),
            
            radioButtons("køn", "Køn",
                         choices = c("Mand", "Kvinde")),
            
            numericInput("tid", "Timer brugt på studiet om ugen",
                         value = NULL),
            
            sliderTextInput("glad", label = "Hvor glad er du i dag?",
                            choices = c("Ked", "Lidt nede", "Middel", "Glad", "Meget glad"),
                           selected = c("Middel")),
            
            actionButton("add", "Tilføj observation")),

        # Show a plot of the generated distribution
        mainPanel(
          
            "Her viser jeg mine plots",
            
            br(),
            br(),
            
            tabsetPanel(id = "tabs",
                        tabPanel("Plot",
                                 br(),
                                 selectInput("xaxis", "Vælg x-akse", choices = names(passat),
                                             selected = "km_per_liter", TRUE, multiple = FALSE),
                                 
                                 selectInput("yaxis", label = "Vælg y-akse", choices = names(passat),
                                             multiple = FALSE),
                                 br(),
                                 plotOutput("plot")),
                        
                        tabPanel("Tabel", tableOutput("tabel")),
                        
                        tabPanel("Corona-plot",
                                 br(),
                                 dateRangeInput(inputId = "rt_date", "Vælg datoer",
                                                start = Sys.Date()-30,
                                                end = Sys.Date(),
                                                max = Sys.Date(),
                                                startview = "month",
                                                weekstart = 1),
                                 plotOutput("covid_plot")),
                        tabPanel("Brugere", 
                                 br(),
                                 actionButton("indlæs", "Indlæs datasæt"),
                                              DTOutput("table_sheet")))
            
        )
    )
)

table_data <- data.frame(bruger = as.character(),
                         date = as.Date(as.character()),
                         fødeby = as.character(),
                         køn = as.character(),
                         tid = as.numeric(),
                         glad = as.character(),
                         check.names = FALSE)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$brugere <- renderUI({
        
        selectizeInput("bruger", label = "Navn",
                   choices = df()$Brugere,
                   options = list(
                       placeholder = "Vælg en bruger",
                       onInitialize = I('function() {this.setValue(""); }')))
    })
    
    
    tableValues <- reactiveValues(
        
        df = data.frame(bruger = as.character(),
                         date = as.Date(as.character()),
                         fødeby = as.character(),
                         køn = as.character(),
                         tid = as.numeric(),
                         glad = as.character(),
                         check.names = FALSE))
    
    
    observeEvent(input$add, {
        
        show_alert(title = "Observation blev tilføjet!",
                   type = "success")
        
        data <- tableValues$ny
        
        newRow <- data.frame(bruger = input$bruger,
                             date = input$date,
                             fødeby = input$fødeby,
                             køn = input$køn,
                             tid = input$tid,
                             glad = input$glad,
                             check.names = FALSE)
        
        
        data <- rbind(data, newRow)
        
        tableValues$ny <- data
        
        sheet_append("113t7xb2VKnjVMJvnmhixSpgelq2w1tTxdvFCapoErr8", newRow, sheet = 1)
        
        })
    
    
    df <- eventReactive(input$indlæs, {
        
        read_sheet("https://docs.google.com/spreadsheets/d/113t7xb2VKnjVMJvnmhixSpgelq2w1tTxdvFCapoErr8/edit#gid=0", range = "Brugere")
        
    })
    
    output$table_sheet <- renderDataTable({
        
        datatable(df())
        
    })
    
    
    
    output$plot <- renderPlot({
        
        passat_adj <- passat %>%
            filter(price <= input$price_adj)
        
        ggplot(data = passat_adj, aes_string(x = as.name(input$xaxis), y = as.name(input$yaxis))) +
            geom_point(size = input$size, alpha = input$alpha) +
            labs(title = paste0("Her er sammenhængen mellem ", as.name(input$xaxis),
                                " og ", as.name(input$yaxis)),
                 subtitle = paste0("Her vises biler der koster mindre end ", input$price_adj, " kroner")) +
            theme(plot.title = element_text(size = 30),
                  plot.subtitle = element_text(size = 20),
                  axis.title = element_text(size = 20))
        
    })
    
    output$tabel <- renderTable({
        
        pas_adj <- passat %>%
            filter(price <= input$price_adj)
        
        print(pas_adj)
        
    })
    
    output$covid_plot <- renderPlot({
        
        rt2 <- rt %>%
            
            filter(SampleDate >= as.Date(input$rt_date[1]) & SampleDate <= as.Date(input$rt_date[2]))
        
        p <- ggplot(data = rt2, aes(x = as.Date(SampleDate), y = estimate)) +
            geom_line() +
            geom_hline(yintercept = 1, color = "red", size = 2, alpha = 0.5) +
            labs(x = "Dato",
                 y = "Kontakttal")
        
        print(p)
        
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
