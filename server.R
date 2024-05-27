## LIBRARIES ###############################################################
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)
library(readr)
library(maps)
library(sf)  # Use sf instead of maptools
library(plotly)
library(RColorBrewer)
library(stringi)
library(shinythemes)


## DATA PREPARATION ######################################################
# Load the dataset
## DATA PREPARATION ######################################################
# Load the dataset
dino_data <- read.csv("data.csv")

# Define color palettes
palette <- c("#291C4A", "#5C2A7A", "#9A3790", "#E55799", "#FA8D6D", "#FEB84C", "#FBE8C5")

# for bar chart
period_colors = c("Jurassic" = "#9A3790",
                  "Cretaceous" = "#FA8D6D",
                  "Triassic" = "#FEB84C")

# for pie chart
diet_colors <- c("carnivorous" = "#291C4A", 
                 "herbivorous" = "#5C2A7A", 
                 "omnivorous" = "#9A3790", 
                 "herbivorous/omnivorous" = "#E55799", 
                 "unknown" = "#FA8D6D")

# Fix values in some rows
row_to_change <- which(dino_data$type == "1.0m")
dino_data$type[row_to_change] <- ""
dino_data$length[row_to_change] <- "1.0m"

row_to_change <- which(dino_data$period == "USA")
dino_data$period[row_to_change] <- ""
dino_data$lived_in[row_to_change] <- "USA"

# Split the period column into 3 new ones
split_period <- strsplit(as.character(dino_data$period), " ")

dino_data$period_stage <- sapply(split_period, function(x) x[1])
dino_data$period_name <- sapply(split_period, function(x) x[2])
dino_data$period_years_ago <- sapply(split_period, function(x) x[3])


# Replace NA values in new columns with empty strings
dino_data$period_stage[is.na(dino_data$period_stage)] <- ""
dino_data$period_name[is.na(dino_data$period_name)] <- ""
dino_data$period_years_ago[is.na(dino_data$period_years_ago)] <- ""


# Change the 'length' column into numeric values
dino_data$length <- as.numeric(sub("m", "", dino_data$length))

# Prepare data for the data table
dino_for_table <- dino_data[c("name", "species", "type", "lived_in", 
                              "diet", "length", "period_name", 
                              "period_stage", "period_years_ago", "named_by")]

colnames(dino_for_table) <- c("Dinosaur Name", "Species", "Type", "Lived In", 
                              "Diet", "Length(m)", "Period", "Period Stage", 
                              "Lived (million years ago)", "Named By")




# Calculate dinosaur counts per country


## SERVER FUNCTION #########################################################
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    dino_data %>% filter(type %in% input$dino_type)
  })
  
  # Overview tab
  output$total_species <- renderValueBox({
    valueBox(value = nrow(dino_data), subtitle = "Number of dinosaurs", icon = icon("globe"), color = "purple")
  })
  
  output$total_types <- renderValueBox({
    valueBox(value = length(stri_omit_empty(unique(dino_data$type), na_empty = FALSE)), subtitle = "Total Types", icon = icon("paw"), color = "blue")
  })
  
  output$total_periods <- renderValueBox({
    valueBox(value = length(stri_omit_empty(unique(dino_data$period_name), na_empty = FALSE)), subtitle = "Number of periods", icon = icon("clock"), color = "green")
  })
  
  
  
  
  
  output$map <- renderLeaflet({
    req(input$period_filter)
    
    dino_fitered <- dino_data[(dino_data$period_name %in% input$period_filter) &
                              (dino_data$diet %in% input$diet_filter),]
    
    dino_fitered <- dino_fitered %>% filter(!is.na(lived_in))
    
    dino_counts <- dino_fitered %>%
      group_by(lived_in) %>%
      summarise(count = n(), .groups = 'drop')
    
    # Load world map as sf object
    world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
    
    # Ensure CRS is WGS84
    world <- st_transform(world, crs = 4326)
    
    # Join dinosaur counts with world map data
    world <- world %>% left_join(dino_counts, by = c("ID" = "lived_in"))
    
    # Define color palette for the polygons
    color_palette <- colorBin(palette = palette, domain = world$count, bins = 7, na.color = "transparent")
    
    leaflet(world) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~color_palette(count),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste0(ID, ": ", ifelse(is.na(count), 0, count), " dinosaurs")
      )
  })
  
  
  # Species Details tab
  output$table <- renderDT({
    
    if (length(input$dino_diet) == 0) {
      filtered_data <- dino_for_table[dino_for_table$Diet %in% input$dino_diet, ]
      
    } else {
    
      diet_pattern <- paste(input$dino_diet, collapse = "|")
      
      if (input$region != "All regions") {
        filtered_data <- dino_for_table[
          (dino_for_table$`Lived In` == input$region | is.na(dino_for_table$`Lived In`)) &
            (grepl(diet_pattern, dino_for_table$Diet) | is.na(dino_for_table$Diet)) &
            (dino_for_table$Period %in% input$dino_period | is.na(dino_for_table$Period)) &
            (!is.na(dino_for_table$`Length(m)`) & dino_for_table$`Length(m)` >= input$dino_length[1]) &
            (!is.na(dino_for_table$`Length(m)`) & dino_for_table$`Length(m)` <= input$dino_length[2]),
        ]
      } else {
        filtered_data <- dino_for_table[
          (grepl(diet_pattern, dino_for_table$Diet) | is.na(dino_for_table$Diet)) &
            (dino_for_table$Period %in% input$dino_period | is.na(dino_for_table$Period)) &
            (!is.na(dino_for_table$`Length(m)`) & dino_for_table$`Length(m)` >= input$dino_length[1]) &
            (!is.na(dino_for_table$`Length(m)`) & dino_for_table$`Length(m)` <= input$dino_length[2]),
        ]
      }
      }
    
    datatable(filtered_data, 
              options = list(rowCallback = JS(
                "function(row, data, index) {",
                "  $(row).on('click', function() {",
                "    Shiny.setInputValue('selected_row', data[0], {priority: 'event'});",
                "  });",
                "}"),
                dom = 't',
                paging = FALSE
              )
    )
  })
  
  observeEvent(input$selected_row, {
    showModal(modalDialog(
      title = paste("Dinosaur", toupper(dino_data[input$selected_row, "name"])),
      HTML(paste("Name:", dino_data[input$selected_row, "name"],
                 "<br>Species:", dino_data[input$selected_row, "species"],
                 "<br>Type:", dino_data[input$selected_row, "type"],
                 "<br>Lived:", dino_data[input$selected_row, "period_years_ago"], "million years ago",
                 "<br><br>Great choice! You have a great taste in dinosaurs :)",
                 "<br><br>If you're interested in more information, you'll find it here:<br>",
                 '<a href="', dino_data[input$selected_row, "link"], '" target="_blank">', 
                 dino_data[input$selected_row, "link"], '</a>'))
    ))
  })
  
  
  
  
  
  
  
  #LENGTH VIOLIN PLOTS
  
  output$value_ui <- renderUI({
    req(input$attribute4)
  })
  
  output$violinPlot <- renderPlotly({
    req(input$attribute4)
    
    filtered_data <- dino_data[(dino_data$diet == "carnivorous" | 
                                  dino_data$diet == "herbivorous" | 
                                  dino_data$diet == "omnivorous") & 
                            
                                 dino_data[[input$attribute4]] != "" & 
                            is.finite(dino_data$length), ]
    
    attribute_label <- switch(input$attribute4,
                              "diet" = "Diet",
                              "period_name" = "Period",
                              "type" = "Dinosaur Type")
    
    p <- ggplot(data=filtered_data, 
                aes_string(x=input$attribute4, 
                           y="length", 
                           fill=input$attribute4, 
                           text=input$attribute4)) + 
      
      geom_violin(drop=FALSE) +
      
      labs(x=attribute_label, 
           y="Length of Dinosaures (m)", 
           title=paste("Distribution of Lengths by", attribute_label),
           fill = attribute_label) + 
      
      theme(axis.text.x = element_text(angle=45, hjust=1, size=13),
            axis.text.y = element_text(size=13),
            axis.title = element_text(size=15),
            plot.title = element_text(size=20, hjust=0.5),
            legend.title = element_text(size=14, hjust=0.5),
            legend.text = element_text(size=13)) +
      
      scale_fill_manual(values=palette) + 
      scale_x_discrete(drop=F)
    
    ggplotly(p, tooltip = c("text"), height = 490)
  })
  #LENGTH IN STAGES
  
  output$interactivePlot <- renderPlotly({
    req(input$attribute3)
    
    stage_order <- c("Early", "Mid", "Late")
    
    filtered_data <- dino_data[dino_data$period_name == input$attribute3, ]
    
    filtered_data$period_stage <- factor(filtered_data$period_stage, 
                                         levels=stage_order)
    
    p <- ggplot(data=filtered_data, 
                aes(x=period_stage, 
                    y=length, 
                    color=period_stage, 
                    text=paste("Name:", name, "\nLength:", length))) + 
      
      geom_jitter(width=0.3, alpha=0.7, size=2) +
      
      labs(x="Period Stage", 
           y="Length of Dinosaures", 
           title=paste("Dinosaurs lengths in different stages of", input$attribute3, "\n"),
           color = "Period Stage") + 
      
      theme(axis.text.x = element_text(angle=45, hjust=1, size=13),
            axis.text.y = element_text(size=13),
            axis.title = element_text(size=15),
            plot.title = element_text(size=20, hjust=0.5),
            legend.title = element_text(size=14, hjust=0.5),
            legend.text = element_text(size=12)
      ) +
      
      scale_color_manual(values=palette) +
      scale_y_continuous(limits = c(0, 35))
    
    ggplotly(p, tooltip = c("text"), height = 490)
    
    
  })
  
  
  #Diets
  output$value_ui <- renderUI({
    req(input$attribute2)
    selectInput("value2", "Value:", choices = unique(dino_data[[input$attribute2]]))
  })
  
  output$pieChart <- renderPlotly({
    req(input$attribute2, input$value2)
    
    filtered_data <- dino_data[dino_data[[input$attribute2]] == input$value2, ]
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    attribute_label <- switch(input$attribute2,
                              "lived_in" = "in",
                              "period_name" = "in Period",
                              "type" = "within type")
    
    
    p <- plot_ly(filtered_data, labels = ~diet, type = 'pie',
                 textinfo = 'label+percent',
                 marker = list(colors=diet_colors,
                               line = list(color = '#FFFFFF', width = 2)),
                 height=490
    )
    
    p <- p %>% layout(
      
      title = list(
        text = paste("Distribution of Diet", attribute_label, input$value2),
        font = list(size=25),
        xanchor = "center",
        yanchor = "top",
        y=0.96
      ),
      
      legend = list(
        font = list(size=15)
      ),
      
      margin = list(
        l=100,
        t=70
      ),
      
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
    
    p
  })
  
  
  #types
  output$barChart <- renderPlotly({
    req(input$attribute1)
    
    filtered_data <- dino_data[dino_data$period_name %in% input$attribute1 &
                                 dino_data$type != "", ]
    
    all_types = c("armoured dinosaur", "ceratopsian", 
                  "euornithopod", "large theropod",
                  "sauropod", "small theropod")
    
    filtered_data <- filtered_data %>%
      mutate(type = factor(type, levels = all_types))
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    proportions_data <- filtered_data %>%
      dplyr::count(type, period_name) %>%
      group_by(period_name) %>%
      mutate(proportion = (n / sum(n))*100) %>%
      ungroup()
    
    p <- ggplot(data=proportions_data, 
                aes(x=type, 
                    y=proportion,
                    fill = period_name,
                    text=paste("Type:", type, "\nPeriod:", period_name, 
                               "\nPercentage:", round(proportion, 2), "%"))) + 
      geom_bar(stat="identity", position = "dodge", width=0.7) +
      
      labs(x="Dinosaur Type", 
           y="Percetage of Dinosaures", 
           title="Distribution of Dinosaur Types",
           fill = "Period Name") + 
      
      theme(axis.text.x = element_text(angle=45, hjust=1, size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size=15),
            plot.title = element_text(size=20, hjust=0.5),
            legend.title = element_text(size=14, hjust=0.5),
            legend.text = element_text(size=12)
      ) +
      
      ylim(0, 55) +
      
      scale_fill_manual(values=period_colors)
    
    ggplotly(p, tooltip = c("text"), height=490)
    
    
  })
}