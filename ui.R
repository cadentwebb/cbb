library(shiny)
library(shinydashboard)
library(markdown)
library(shinycssloaders)

ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "Caden's Prediction Engine", titleWidth = 300),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(title = "Select Inputs", 
                            selectInput("position", "Player Position", choices = c("PG", "SG", "SF", "PF", "C")),
                            selectInput("conference",
                                        "Future Conference",
                                        choices = c("Sun Belt", "Big Ten", "Southeastern", "Big East",
                                                    "Atlantic Coast", "Southland", "America East", "Mid-American",
                                                    "Pacific 12", "Mountain West", "Ivy", "Missouri Valley", "Conference USA",
                                                    "Big Sky", "Big West", "Horizon", "Big 12", "Patriot League", "West Coast",
                                                    "Atlantic 10", "Ohio Valley", "Southern", "Western Athletic", "Colonial",
                                                    "Atlantic Sun", "Big South", "Metro Atlantic Athletic", "Southwestern Athletic",
                                                    "Northeast"))),
                        #box(plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls",
                            sliderInput("rank",
                                        "247 Composite Ranking",
                                        min = 0.7,
                                        max = 0.9999,
                                        value = "0.8"),
                            sliderInput("height",
                                        "Height (Inches)",
                                        min = 60,
                                        max = 90,
                                        value = "72"))
                    ),
                    fluidRow(
                        valueBoxOutput("pointbox", width = 3),
                        valueBoxOutput("reboundsbox", width = 3),
                        valueBoxOutput("assistbox", width = 3),
                        valueBoxOutput("usagebox", width = 3)
                    ),
                    fluidRow(
                        tabBox(id = "tabset1",
                            tabPanel("All",  withSpinner(plotOutput("pointsplot"),type = 5)),
                            tabPanel("Position", withSpinner(plotOutput("pointsplot_pos"), type = 5)), width = 3),
                        tabBox(id = "tabset2",
                               tabPanel("All", withSpinner(plotOutput("reboundsplot"), type = 5)),
                               tabPanel("Position", withSpinner(plotOutput("reboundsplot_pos"), type = 5)), width = 3),
                        tabBox(id = "tabset3",
                               tabPanel("All", withSpinner(plotOutput("assistsplot"), type = 5)),
                               tabPanel("Position", withSpinner(plotOutput("assistsplot_pos"), type = 5)), width = 3),
                        tabBox(id = "tabset4",
                               tabPanel("All", withSpinner(plotOutput("usageplot"), type = 5)),
                               tabPanel("Position", withSpinner(plotOutput("usageplot_pos"), type = 5)), width = 3)
                
            )
            ),
            
            # Second tab content
            tabItem(tabName = "about",
                    fluidRow(
                        tabBox(id = "tabset5",
                               tabPanel("Overview", includeMarkdown("Overview.md")),
                               tabPanel("Data", withMathJax(includeMarkdown("Data.md"))),
                               tabPanel("Modeling", withMathJax(includeMarkdown("Modeling.md"))),
                               tabPanel("Output Info", includeMarkdown("Output.md")),
                               tabPanel("Future Ideas", includeMarkdown("Future.md")),
                               width = 12)
                    )
            )
        )
    )
)
#Dropdown button    