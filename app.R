#
# This is a Shiny dashboard using the COVID19 package.
#
#!/usr/bin/env Rscript

library(devtools)
#devtools::install_github("nik01010/dashboardthemes")
library(tidyverse)
library(shiny)
library(shinydashboard)
library(COVID19)
library(dashboardthemes)


us <- covid19(country = "US", level = 2, raw = TRUE)
territories <- c("Guam", "American Samoa", "Northern Mariana Islands", "Virgin Islands", "Puerto Rico")

ST <- us %>% 
    ungroup() %>%
    filter(!grepl(paste(territories, collapse="|"), administrative_area_level_2)) %>% 
    distinct(administrative_area_level_2) %>% 
    arrange(administrative_area_level_2) %>% 
    pull()

counties <- covid19(country = "US", level = 3, raw = TRUE)


source("theme_diy.R")


ui <- dashboardPage(
    dashboardHeader(
        title = logo_nord
    ),
    dashboardSidebar(disable = TRUE),
                     
    dashboardBody(
        
            theme_nord, 
        fluidRow(
            box(
                selectInput("state", h2("Select a State"), ST)
            ),
            box(
                width = 5,
                align = "center",
                title = "Total Confirmed Cases in the US",
                h1(textOutput("total_cases")),
                solidHeader = TRUE,
                status = "warning"
            )
        ),
        fluidRow(
                    box(width = 6,
                        title = "Confirmed Cases",
                        plotOutput("cases_chloro")
                        ),
                    box(width = 6,
                        title = "Total Deaths in Each County",
                        plotOutput("deaths_cholor")
                        )
                ),
        
                fluidRow(
                    box(width = 4,
                        align = "center",
                        title = "Number of Tests in the State",
                        h1(textOutput("N_tests")), 
                        status = "success",
                        solidHeader = TRUE
                    ),
                    box(
                        width = 4,
                        align = "center",
                        title = "Number of Cases in the State",
                        h1(textOutput("N_confirmed")), 
                        status = "info",
                        solidHeader = TRUE
                    ),
                    box(width = 4,
                        align = "center",
                        title = "Deaths",
                        h1(textOutput("N_deaths")),
                        status = "danger",
                        solidHeader = TRUE
                        
                    )),
                
                fluidRow(
                       box(
                           width = 6,
                           title = "Number of confirmed cases over time",
                           plotOutput("all_cases")
                       ),
                       box(
                           width = 5,
                           title = "Number of confirmed cases over the past week",
                           plotOutput("recent_cases")
                       )
                       
                       )
                )
    )


server <- shinyServer(function(input, output, session) {
    
    output$total_cases <- renderText({
        us %>% 
            ungroup() %>% 
            group_by(administrative_area_level_2) %>% 
            slice(n()) %>% 
            ungroup() %>% 
            summarise(sum(confirmed, na.rm = TRUE)) %>% 
            pull() %>% 
            prettyNum(big.mark = ",")
            
    })
    
    output$N_confirmed <- renderText({
        us %>% 
            ungroup() %>% 
            group_by(administrative_area_level_2) %>% 
            filter(str_detect(administrative_area_level_2, input$state)) %>% 
            tail(1) %>% 
            select(confirmed) %>% 
            pull() %>% 
            prettyNum(big.mark = ",")
    })
    
    
    output$N_deaths <- renderText({
        us %>% 
            ungroup() %>% 
            group_by(administrative_area_level_2) %>% 
            filter(str_detect(administrative_area_level_2, input$state)) %>% 
            tail(1) %>% 
            select(confirmed) %>% 
            pull() %>% 
            prettyNum(big.mark = ",")
    })
    output$N_tests <- renderText({
        us %>% 
            ungroup() %>% 
            group_by(administrative_area_level_2) %>% 
            filter(str_detect(administrative_area_level_2, input$state)) %>% 
            tail(1) %>% 
            select(confirmed) %>% 
            pull() %>% 
            prettyNum(big.mark = ",")
    })

    output$cases_chloro <- renderPlot({
        st <- counties %>% 
            ungroup() %>% 
            filter(str_detect(administrative_area_level_2, input$state)) %>% 
            arrange(administrative_area_level_3) %>% 
            rename(county_name = administrative_area_level_3) %>% 
            select(county_name, date, tests, confirmed, deaths, recovered) %>% 
            group_by(county_name) %>% 
            slice(n()) %>% 
            rename(Cases = "confirmed")
        
        urbnmapr::counties %>% 
            filter(state_name ==  input$state) %>% 
            mutate(county_name = str_remove_all(county_name, " County")) %>% 
            left_join(st, by = "county_name") %>% 
            ggplot(mapping = aes(long, lat, group = group, fill = Cases)) +
            geom_polygon(color = "white") +
            coord_map() +
            scale_fill_gradient(low = "#ECEFF4", high = "#A3BE8C")+
            theme_void()
    })
    
    
    output$deaths_cholor <- renderPlot({
        st <- counties %>% 
            ungroup() %>% 
            filter(str_detect(administrative_area_level_2, input$state)) %>% 
            arrange(administrative_area_level_3) %>% 
            rename(county_name = administrative_area_level_3) %>% 
            select(county_name, date, tests, confirmed, deaths, recovered) %>% 
            group_by(county_name) %>% 
            slice(n()) %>% 
            rename(Cases = "deaths")
        
        urbnmapr::counties %>% 
            filter(state_name ==  input$state) %>% 
            mutate(county_name = str_remove_all(county_name, " County")) %>% 
            left_join(st, by = "county_name") %>% 
            ggplot(mapping = aes(long, lat, group = group, fill = Cases)) +
            geom_polygon(color = "white") +
            coord_map() +
            scale_fill_gradient(low = "#ECEFF4", high = "#D08770")+
            theme_void()
        
    })
    
    
    output$all_cases <- renderPlot({
        con.ave <- us %>% 
            filter(str_detect(administrative_area_level_2, input$state)) %>% 
            ungroup() %>% 
            summarise(mean(confirmed)) %>% 
            pull()
        us %>% 
            filter(str_detect(administrative_area_level_2, input$state)) %>% 
            ungroup() %>% 
            select(date, confirmed) %>% 
            ggplot(aes(x = date, y = confirmed)) +
            geom_col(fill = "#5E81AC") +
            theme_classic() +
            labs(x = "", y = "Number of Confirmed Cases")
        
    })
    
    output$recent_cases <- renderPlot({
        
        us %>% 
            filter(str_detect(administrative_area_level_2, input$state)) %>% 
            ungroup() %>% 
            arrange(desc(date))  %>% 
            slice(1:7) %>% 
            select(date, confirmed) %>% 
            ggplot(aes(x = date, y = confirmed)) +
            geom_line(color = "#B48EAD", size = 1)  +
            geom_point(color = "#5E81AC", size = 3.5) +
            theme_classic() +
            labs(x = "", y = "Number of Confirmed Cases")
        
        
        
    })
    
})

shinyApp(ui, server)