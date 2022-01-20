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
                             valueBoxOutput(width = 3, "tp")))
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
        
        p_p <- mean(pos1$PosPct)
        
        p <- round(p_p, digits = 2)
        
        valueBox(value = (paste0(p, " %")), "Positiv % for rullende syv dage", icon = icon("percent"), color = "blue")
        
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)

