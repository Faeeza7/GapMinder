library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(shinythemes)
library(rsconnect)

library(readr)
gapminder <- read_csv("C:/Users/Admin/Documents/gp/gapminder.csv")
View(gapminder)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "GapMinder Analysis",
                                    
                                    tags$li(class="dropdown",tags$a(href="https://www.youtube.com/watch?v=kBVSQHUl30s",icon("youtube"),"gapminder",target="_blank",
                                                                    tags$li(class="dropdown",tags$a(href="https://www.gapminder.org/",icon ("Webpage"),"About",target="_blank"))))
                                    
                                    
                    ),
                    
                    dashboardSidebar(sidebarMenu(id = "tab1", selected = "Gapminder",
                                                 menuItem("Gapminder", tabName = "Gapminder",icon=icon("database")),
                                                 menuItem("GDP", tabName = "gdpPerCap",icon=icon("chart-line")),
                                                 
                                                 menuItem("POPULATION", tabName = "population",icon=icon("chart-line")))),
                    dashboardBody(
                      tabItems(
                        tabItem("Gapminder",icon=icon("chart-line"),
                                fluidRow(column(12, h1("About"),
                                                img(src = "gapm.png", height =50, width = 400,
                                                    style="float: block; margin-left: auto; margin-right: auto;"),
                                                h3("Data included in this dataset is from the Gapminder.org website."),
                                                h3("It was inspired by Hans Rosling visualization and decided to create my own visualization with pandas and seaborn.
                                  Gapminder (Factfulness graph"),
                                                h3("It provides data about the population,
                                  life expectancy and GDP in different countries of the world from 1952 to 2007.
                                  There is also a separate file for 2007.")))),
                        
                        tabItem("gdpPerCap",icon=icon("chart-line"),
                                
                                
                                fluidRow(column(12, plotlyOutput("g")),
                                         
                                         fluidRow(column(6,offset=1,fluidRow(selectizeInput(
                                           "select1", "Please select the continent of choice", multiple = FALSE,
                                           choices = sort(unique(gapminder$continent)),
                                           selected = sort(unique(gapminder$continent)))),
                                           
                                           
                                           fluidRow(sliderInput(
                                             "Slider1", "Please select the year",
                                             min = floor(min(gapminder$year)),
                                             max = ceiling(max(gapminder$year)),
                                             value = c(min = min(gapminder$year), max = max(gapminder$year)),animate=TRUE,
                                           )),
                                           
                                           )
                                         ),
                                         fluidRow(column(12,offset=0,dataTableOutput("ao"))),
                                         
                                         
                                )),
                        tabItem("population",
                                
                                
                                fluidRow(column(12, plotlyOutput("p")),
                                         
                                         fluidRow(column(6,offset=1,fluidRow(selectizeInput(
                                           "select3", "Please select the continent of choice", multiple = FALSE,
                                           choices = sort(unique(gapminder$continent)),
                                           selected = sort(unique(gapminder$continent)))),
                                           
                                           fluidRow(sliderInput(
                                             "Slider2", "Please select the year",
                                             min = floor(min(gapminder$year)),
                                             max = ceiling(max(gapminder$year)),
                                             value = c(min = min(gapminder$year), max = max(gapminder$year)),animate = TRUE,
                                             
                                           )),
                                           
                                         )
                                         
                                         
                                         ),
                                         fluidRow(column(12,offset=0,dataTableOutput("a1"))),  
                                )
                                
                        )
                      )
                    )
)
server <- function(input, output, session){
  
  
  output$g <- renderPlotly({
    pdata0 <- gapminder[which(gapminder$continent %in% input$select1),]
    
    pdata <- pdata0[which(pdata0$year %in% (input$Slider1[1]:input$Slider1[2])),]
    ggplot(pdata,aes(x=gdpPerCap,y=lifeExp,fill=country))+
      geom_point(size=5,alpha=0.5)
  })
  
  output$p <- renderPlotly({
    pdata0 <- gapminder[which(gapminder$continent %in% input$select3),]
    
    pdata <- pdata0[which(pdata0$year %in% (input$Slider2[1]:input$Slider2[2])),]
    ggplot(pdata,aes(x=population,y=lifeExp,fill=country))+
      geom_point(size=5,alpha=0.5)
  })
  
  output$ao <- renderDataTable({
  df = as.data.frame(gapminder %>% 
                       group_by(country,continent) %>% 
                       summarise(t=n(),
                                 MEANlifeExp=round(100*mean(lifeExp)),
                                 MEANgdpPerCap=round(100*mean(gdpPerCap)),
                                 variance=round(100*var(lifeExp,gdpPerCap))))
  
  datatable(df,options = list(searching = TRUE))
  })
  
  
  output$a1 <- renderDataTable({
    df = as.data.frame(gapminder %>% 
                         group_by(country,continent) %>% 
                         summarise(t=n(),
                                   MEANlifeExp=round(100*mean(lifeExp)),
                                   MEANpopulation=round(100*mean(population)),
                                   variance=round(100*var(lifeExp,population))))
    
    datatable(df,options = list(searching = TRUE))
  })
  
}

shinyApp(ui, server)