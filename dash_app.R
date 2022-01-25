library(shiny)
library(shinydashboard)
library(tidyverse)

rt <- read_csv2("Rt_cases_2022_01_18.csv")

na <- read_csv2("Newly_admitted_over_time.csv")

tp <- read_csv2("Test_pos_over_time.csv")

dt <- read_csv2("Deaths_over_time.csv")



# UI ----------------------------------------------------------------------



ui <- dashboardPage(
    
    title = "Covid-19 dashboard - Dataanalyse 2022", skin = "red",

    dashboardHeader(title = div(img(height = 50, width = 50, src = "corona.png"))),
    
    dashboardSidebar(
        
        collapsed = FALSE,
        
        sidebarMenu(id = "menu",
                    menuItem("Danmarks nøgletal", tabName = "nøgletal", icon = icon("viruses"),
                             menuSubItem("København", tabName = "kbh", icon = icon("city")),
                             menuSubItem("Nordjylland", tabName = "njy")),
                    menuItem("Sveriges nøgletal", tabName = "sv_nøgletal", icon = icon("viruses")))
    
    ),
    
    dashboardBody(
        
        tabItems(
            
            tabItem(tabName = "kbh",
                    
                    fluidRow(valueBoxOutput(width = 3, "rt"),
                             valueBoxOutput(width = 3, "tp")),
                    
                    fluidRow(
                        
                        tabBox(title = "Tendens", id = "tendens", width = 12,
                               
                               tabPanel("Kontakttal",
                                        fluidRow(column(width = 4, box(title = "Vælg datoer", status = "primary", width = NULL,
                                                                       solidHeader = TRUE,
                                                                       
                                                                       dateRangeInput(inputId = "rt_date", "Vælg datoer",
                                                                                      start = Sys.Date(),
                                                                                      end = Sys.Date(),
                                                                                      max = Sys.Date(),
                                                                                      startview = "month",
                                                                                      weekstart = 1),
                                                                       
                                                                       checkboxInput("trend_line", label = "Plot en tendslinje", value = TRUE),
                                                                       
                                                                       ))),
                                        
                                        fluidRow(column(width = 12, box(title = "Kontakttal over tid", status = "primary", width = NULL,
                                                        plotOutput("rt_tendens"))))))))
        )
        
    )
    
)
    




server <- function(input, output) {
    
    output$rt <- renderValueBox({
        
        last_row <- tail(rt, n = 1)
        
        valueBox(value = last_row$estimate, "Kontakttal",
                 subtitle = paste0("Kontakttal pr. ", last_row$SampleDate),icon = icon("people-arrows"), color = "yellow")
        
    })
    
    output$tp <- renderValueBox({
        
        pos1 <- tp %>%
            
            slice_tail(n = 9) %>%
            
            slice_head(n = 7)
        
        p_p <- mean(pos1$PosPct) %>%
            round(., digits = 2)
        
        
        valueBox(value = (paste0(p_p, " %")), "Positiv % for rullende syv dage", icon = icon("percent"), color = "blue")
        
    })
    
    
    output$rt_tendens <- renderPlot({
        
        rt2 <- rt %>%
            
            filter(SampleDate >= as.Date(input$rt_date[1]) & SampleDate <= as.Date(input$rt_date[2]))
        
        p <- ggplot(rt2, aes(as.Date(SampleDate), estimate)) +
            geom_line() +
            geom_hline(yintercept = 1, color = "red", size = 2, alpha = 0.5) +
            labs(x = "Dato",
                 y = "Kontakttal")
        
        if(input$trend_line) {
            
            p <- p + geom_smooth(se = FALSE)
        }
        
        print(p)
    
    })


        
}

# Run the application 
shinyApp(ui = ui, server = server)

