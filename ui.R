library(shiny)
library(shinydashboard)



#UI for the dashboard
ui <- dashboardPage(skin="green",
                    dashboardHeader(title = tags$div(
                      style = "display: flex; position: fixed; align-items: center;",
                      tags$div(
                        tags$img(src = "tyrannosaurus-rex.png", height = "30px", style = "margin-right: 10px;"),
                        style = "flex-shrink: 0;"
                      ),
                      tags$div(
                        tags$span("Dinosaur Explorer", style = "font-size: 20px; white-space: nowrap;")
                      )
                    )),
                    dashboardSidebar(
                      tags$head(
                        tags$style(HTML("
        .main-sidebar {
          position: fixed;
          max-height: 100%;
          overflow: hidden;
        }
        .content-wrapper, .right-side, .main-footer {
          margin-left: 230px; /* Adjust based on the sidebar width */
        }
        .sidebar-menu, .main-sidebar .sidebar {
          position: static;
          height: auto;
          overflow: visible;
        }
      "))
                      ),
                      tags$div(
                        style = "display: flex; flex-direction: column; height: 100%;",
                        tags$div(
                          style = "flex-grow: 1;",
                          sidebarMenu(
                            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                            menuItem("Find Your Dinosaur", tabName = "details", icon = icon("table")),
                            menuItem("Length of the dinosaurs", tabName = "leng", icon = icon("ruler")),
                            menuItem("Diet of the dinosaurs", tabName = "diet", icon = icon("apple-whole")),
                            menuItem("Types of the dinosaurs", tabName = "types", icon = icon("star")),
                            menuItem("About", tabName = "info", icon = icon("circle-info"))
                          )
                        ),
                        tags$div(
                          style = "padding: 5px; text-align: center;",
                          br(), br(),
                          tags$img(src = "dinosaur-fossil.png", height = "90px"),
                          br(), br(), br(), br(),
                          tags$img(src = "PP_monogram.png", height = "90px"),
                          br(), br(), br(), br(),
                          tags$img(src = "dinosaur-fossil.png", height = "90px"),
                          
                        )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "overview",
                                h2("Overview", style = "text-align: center;"),
                                fluidRow(
                                  valueBoxOutput("total_species"),
                                  valueBoxOutput("total_types"),
                                  valueBoxOutput("total_periods")
                                ),
                                fluidRow(
                                  box(title = "Map of dinosaurs distribution:", br(),
                                      width = 12,
                                      status = "info",
                                      collapsible = TRUE,
                                      fluidRow(title = "Select Dinosaur Period",
                                               column(width = 3,
                                                      fluidRow(
                                                        box(title = "Select the period that the dinosaurs have lived in:", status = "success",
                                                            width = NULL,
                                                            collapsible = TRUE, style = "margin-left: 5px;",
                                                            checkboxGroupInput("period_filter", label = NULL,
                                                                               choices = c("Jurassic", "Cretaceous", "Triassic"),
                                                                               selected = c("Jurassic", "Cretaceous", "Triassic"))
                                                        )),
                                                      fluidRow(box(title = "Select the diet that the dinosaurs have been on:", status = "success",
                                                                   width = NULL,
                                                                   collapsible = TRUE, style = "margin-left: 5px;",
                                                                   checkboxGroupInput("diet_filter", label = NULL,
                                                                                      choices = c("carnivorous", "herbivorous", "omnivorous"),
                                                                                      selected = c("carnivorous", "herbivorous", "omnivorous"))
                                                      )), offset = 0),
                                               column(width = 9,
                                                      leafletOutput("map", height = 500), offset = 0)
                                      )
                                  )
                                )
                        ),
                        tabItem(tabName = "details",
                                h2("Find your dinosaur!", style = "text-align: center;"),
                                h3("Click to learn more!", style = "text-align: center;"),
                                box(width = NULL, status = "success",
                                    fluidRow(
                                      box(status = "info",
                                          width = 4,
                                          title = "Select places of origin",
                                          selectInput("region", label = NULL, choices = c("All regions", unique(dino_data$lived_in)))
                                      ),
                                      box(status = "info",
                                          width = 4,
                                          title = "Select dinosaur diets",
                                          checkboxGroupInput("dino_diet", label = NULL, choices = c("carnivorous", "herbivorous", "omnivorous"), selected = c("carnivorous", "herbivorous", "omnivorous"))
                                      ),
                                      box(status = "info",
                                          width = 4,
                                          title = "Select period of dinosaur living",
                                          checkboxGroupInput("dino_period", label = NULL, choices = c("Jurassic", "Cretaceous", "Triassic"), selected = c("Jurassic", "Cretaceous", "Triassic"))
                                      )
                                    ),
                                    fluidRow(
                                      box(status = "info",
                                          width = 12,
                                          title = "Choose dinosaur length",
                                          sliderInput("dino_length", label = NULL, min = 0, max = ifelse(is.numeric(max(dino_data$length, na.rm = TRUE)), max(dino_data$length, na.rm = TRUE), 100), value = c(0, ifelse(is.numeric(max(dino_data$length, na.rm = TRUE)), max(dino_data$length, na.rm = TRUE), 100)), step = 0.5)
                                      )
                                    ),
                                    fluidRow(
                                      box(status = "info",
                                          width = 12,
                                          DTOutput('table')
                                      )
                                    )
                                )
                        ),
                        tabItem(tabName = "leng",
                                h2("Length of the dinosaurs", style = "text-align: center;"),
                                box(width = NULL, status = "success", collapsible = TRUE,
                                    fluidRow(
                                      box(
                                        width = 12,
                                        title = "Distribution of length by:",
                                        div(style = "text-align: center;",
                                            radioButtons("attribute4", label = NULL,
                                                         choices = c("Diet" = "diet",
                                                                     "Period" = "period_name",
                                                                     "Dinosaur type" = "type"),
                                                         inline = TRUE)
                                        )
                                      )
                                    ),
                                    fluidRow(
                                      box(
                                        height = 450,
                                        width = 12,
                                        plotlyOutput("violinPlot")
                                      )
                                    )
                                ),
                                br(), br(),
                                box(width = NULL, status = "success", collapsible = TRUE,
                                    fluidRow(
                                      box(
                                        width = 12,
                                        title = "Select Period:",
                                        div(style = "text-align: center;",
                                            radioButtons("attribute3", label = NULL,
                                                         choices = c("Jurassic", "Cretaceous", "Triassic"),
                                                         inline = TRUE)
                                        )
                                      )
                                    ),
                                    fluidRow(
                                      box(
                                        height = 450,
                                        width = 12,
                                        plotlyOutput("interactivePlot")
                                      )
                                    )
                                ),
                                br(), br()
                        ),
                        tabItem(tabName = "diet",
                                h2("Diet of the dinosaurs", style = "text-align: center;"),
                                box(width = NULL, status = "success",
                                    fluidRow(
                                      box(
                                        width = 12,
                                        title = "Select Attribute and Value:",
                                        fluidRow(
                                          column(width = 6,
                                                 selectInput("attribute2", label = "Attribute:",
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
                                        width = 12,
                                        plotlyOutput("pieChart")
                                      )
                                    )
                                )
                        ),
                        tabItem(tabName = "types",
                                h2("Types of the dinosaurs", style = "text-align: center;"),
                                box(width = NULL, status = "success",
                                    fluidRow(
                                      box(
                                        width = 12,
                                        title = "Select Period:",
                                        checkboxGroupInput("attribute1", label = NULL,
                                                           choices = c("Cretaceous", "Jurassic", "Triassic"),
                                                           selected = c("Cretaceous", "Jurassic", "Triassic"),
                                                           inline = TRUE)
                                      )
                                    ),
                                    fluidRow(
                                      box(
                                        width = 12,
                                        plotlyOutput("barChart")
                                      )
                                    )
                                )
                        ),
                        tabItem(tabName = "info",
                        
                                fluidRow(
                                  column(width = 12,
                                         h2("About This Dashboard", style = "text-align: center;"),
                                         br(),
                                         h2("Welcome to the Dinosaur Period Distribution Dashboard!"),
                                         p("This interactive dashboard is designed to provide an insightful and engaging 
                                           view of the distribution of dinosaur species across different geological periods 
                                           and regions with regard to their characteristics. By leveraging dynamic data 
                                           visualization tools, we aim to make the exploration of prehistoric life both 
                                           educational and more importantly enjoyable."),
                                         
                                         h3("Key Features:"),
                                         tags$ul(
                                           tags$li("Period Selection: Use the filter options to select different geological 
                                                   periods, including the Jurassic, Cretaceous, and Triassic. 
                                                   This allows you to explore how dinosaur populations changed over time."),
                                           tags$li("Geographical Distribution: Visualize the regions where different dinosaur species lived. 
                                                   The map highlights countries and regions with varying concentrations of dinosaur discoveries."),
                                           tags$li("Interactive Map: Our map is powered by Leaflet, providing an intuitive and interactive 
                                                   way to explore the data. Hover over regions to see detailed information about 
                                                   the number of dinosaur species discovered there."),
                                           tags$li("Diet Analysis: Explore the dietary habits of different dinosaur 
                                                   species with a pie charts that illustrate the distribution of carnivorous, herbivorous, 
                                                   omnivorous, and other dietary types depending on their characteristics."),
                                           tags$li("Length Distribution: Violin plots and scatter plots provide a visual representation 
                                                   of the length of dinosaurs across different dietary categories, types of the reptiles, 
                                                   geological periods and their different stages."),
                                           tags$li("Type Distribution: Bar charts show the distribution of different types of 
                                                   dinosaurs across our three main periods mentioned above.")
                                         ),
                                         
                                         h3("Data Sources:"),
                                         p("The data presented in this dashboard is curated from dinosaur dataset:"),
                                         p("kaggle: Jurassic Park - The Exhaustive Dinosaur Dataset"),
                                         
                                         h3("How to Use:"),
                                         tags$ol(
                                           
                                           tags$li("Explore the Map: The map will update based on your selection of geological period and the diet, 
                                                   displaying the regions where the given dinosaurs have been found."),
                                           tags$li("Detailed Information in Form of a Find Your Dinosaur table: Choose the values of the atrributes 
                                           that you are interested, like the place of origin, diet, geological period and thelength to find a dinosars that you might be 
                                           interested in; then click on a one to  see more detailed information about 
                                                   it- including the link to the website that can tell you more about the given reptile."),
                                           tags$li("Examine Lengths: Violin and scatter plots show the distribution of dinosaur lengths, 
                                                   allowing for comparisons across different geological periods and their stages, types and dietary habits."),
                                           tags$li("Analyze Diets: Use a pie chart to see the dietary distribution of dinosaurs based on selected countries of 
                                                   their founding, period that they were living in or their type."),
                                           
                                           tags$li("Study Types: Bar charts illustrate the proportions of various dinosaur types within the selected periods.")
                                         ),
                                         
                                         h3("Purpose:"),
                                         tags$ul(
                                           tags$li("Educate: Provide a comprehensive and accessible way to learn about the distribution of dinosaurs."),
                                           tags$li("Engage: Use interactive elements to make learning about prehistoric life more engaging and fun not only for the dinosaur fans."),
                                           tags$li("Inform: Serve as a resource for students, educators, researchers, and anyone interested in paleontology.")
                                         ),
                                         p("We hope you find this dashboard both informative and enjoyable."),
                                         h4("Authors:"),
                                         tags$ul(
                                           tags$li("Magdalena Augustyniak id.156036"),
                                           tags$li("Martyna Stasiak id.156071"),
                                           
                                           
                                         ),
                                         tags$div(
                                           style = "padding: 5px; text-align: center;",
                                           br(), br(),
                                           tags$img(src = "dinosaur-fossil.png", height = "120px")
                                         )
                                  )
                                
                        
                        )
                      )
                    ))
)
