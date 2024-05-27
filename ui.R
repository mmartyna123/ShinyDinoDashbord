library(shiny)
library(shinydashboard)

# Define UI for the dashboard
ui <- dashboardPage(skin="green",
  dashboardHeader(title = tags$div(
    style = "display: flex; align-items: center;",
    tags$div(
      tags$img(src = "tyrannosaurus-rex.png", height = "30px", style = "margin-right: 10px;"),
      style = "flex-shrink: 0;"
    ),
    tags$div(
      tags$span("Dinosaur Explorer", style = "font-size: 20px; white-space: nowrap;")
    )
  )
  ),
  dashboardSidebar(
    tags$div(
      style = "display: flex; flex-direction: column; height: 100%;",
      tags$div(
        style = "flex-grow: 1;",
        sidebarMenu(
          menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
          menuItem("Find Your Dinosaur", tabName = "details", icon = icon("table")),
          menuItem("Length of the dinosaurs", tabName = "leng", icon = icon("ruler")),
          menuItem("Diet of the dinosaurs", tabName = "diet", icon = icon("apple-whole")),
          menuItem("Types of the dinosaurs", tabName = "TYPES", icon = icon("star")),
          menuItem("About", tabName = "Info", icon = icon("circle-info"))
        )
      ),
      br(),
      tags$div(
        style = "padding: 10px; text-align: center;",
        tags$img(src = "PP_monogram.png", height = "90px"),
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              h2("Overview"),
              fluidRow(
                valueBoxOutput("total_species"),
                valueBoxOutput("total_types"),
                valueBoxOutput("total_periods")
              ),
              fluidRow(
                box( title="Map of dinosaurs distribution:", br(),
                     width=12,
                     status = "info",
                     collapsible = TRUE,
                     
                    
                     
                     fluidRow(title = "Select Dinosaur Period",
                       column(width = 3,
                              
                              checkboxGroupInput("period_filter", label = NULL, 
                                                 choices = c("Jurassic", "Cretaceous", "Triassic"),
                                                 selected = c("Jurassic", "Cretaceous", "Triassic")
                                                 )
                       ),
                       column(width = 9,
                              leafletOutput("map", height = 500)
                     )
                )
                  
                  
                    
                  
                )
                #box(
                  #width = 4,
                  #title = "Select Dinosaur Diet",
                 # checkboxGroupInput("diet_filter", label = NULL, 
                #                     choices = unique(dino_data$diet), 
                 #                    selected = unique(dino_data$diet))
                #)
              )
              
      ),
      tabItem(tabName = "details",
              fluidRow(
                box(
                  width = 6,
                  title = "Select places of origin",
                  selectInput("region", label = NULL, choices = c("All regions", unique(dino_data$lived_in)))
                ),
                box(
                  width = 3,
                  title = "Select dinosaur diets",
                  checkboxGroupInput("dino_diet", label = NULL, choices = c("carnivorous", "herbivorous", "omnivorous"), selected = c("carnivorous", "herbivorous", "omnivorous"), inline = TRUE)
                ),
                box(
                  width = 3,
                  title = "Select period of dinosaur living",
                  checkboxGroupInput("dino_period", label = NULL, choices = c("Jurassic", "Cretaceous", "Triassic"), selected = c("Jurassic", "Cretaceous", "Triassic"), inline = TRUE)
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Choose dinosaur length",
                  sliderInput("dino_length", label = NULL, min = 0, max = ifelse(is.numeric(max(dino_data$length, na.rm = TRUE)), max(dino_data$length, na.rm = TRUE), 100), value = c(0, ifelse(is.numeric(max(dino_data$length, na.rm = TRUE)), max(dino_data$length, na.rm = TRUE), 100)), step = 0.5)
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "FIND YOUR DINOSAUR! Click to learn more!",
                  DTOutput('table')
                )
              )),
      tabItem(tabName = "leng",
              h2("Length of the dinosaurs"),
              fluidRow(
                box(
                  width=12,
                  title="Distribution of length by:",
                  
                  radioButtons("attribute4", label=NULL,
                               choices = c("Diet" = "diet", 
                                           "Period" = "period_name", 
                                           "Dinosaur type" = "type"),
                               
                               inline=TRUE)
                )),
              
              fluidRow(
                box(
                  height = 450,
                  width=12,
                  
                  plotlyOutput("violinPlot")
                )),
              br(),
              br(),
              br(),
              fluidRow(
                box(
                  width=12,
                  title="Select Period:",
                  radioButtons("attribute3", label=NULL,
                               choices = c("Jurassic", "Cretaceous", "Triassic"),
                               inline=TRUE)
                )),
              
              fluidRow(
                box(
                  height = 450,
                  width=12,
                  
                  plotlyOutput("interactivePlot")
                )
              ), br(),
              br(),
    ),
    tabItem(tabName = "diet",
            h2("Diet of the dinosaurs"),
            fluidRow(
              box(
                width=12,
                title="Select Attribute and Value:",
                fluidRow(
                  column(width = 6,
                         selectInput("attribute2", label="Attribute:",
                                     choices = c("Place of living" = "lived_in", 
                                                 "Period" = "period_name", 
                                                 "Dinosaur type" = "type"))
                  ),
                  column(width = 6,
                         uiOutput("value_ui"))
                )
              )
            ),
            
            fluidRow(
              box(
                width=12,
                plotlyOutput("pieChart")
              ))
    ),
    tabItem(tabName = "TYPES",
            h2("Types"),
            fluidRow(
              box(
                width=6,
                title="Select Period:",
                checkboxGroupInput("attribute1", label = NULL, 
                                   choices = c("Cretaceous", "Jurassic", "Triassic"), 
                                   selected = c("Cretaceous", "Jurassic", "Triassic"), 
                                   inline = TRUE)
              )),
            
            fluidRow(
              box(
                width=12, 
                plotlyOutput("barChart")
              )
            )
  )
)
)
)

