### Auto Install Required Packages ###

list.of.packages <- c("tidyverse", "ggthemes", "tidygraph", "shinyWidgets", "scales", "shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}


require(tidyverse)
require(ggthemes)
require(tidygraph)
library(shinyWidgets)
require(scales)

data <- read_csv("shiny_data.csv")



load("countries_net.R")

default_background_color <- "#f5f5f2"



ui <- fluidPage(
  setBackgroundColor(default_background_color),
  
  
  titlePanel('Marine Biodiversity Country Publications Dashboard'),
  
  selectInput(inputId = "country",
              label = "Choose a Country",
              selected = "USA",
              choices = sort(unique(data$country_fa))),
  plotOutput(outputId = "barchart_clusters"),
  plotOutput(outputId = "top_5_clusters"),
  tableOutput(outputId = "top_collab")
  
)


server <- function(input, output, session){
  
  output$barchart_clusters <- renderPlot({
    

    country_name <- input$country

    regional_dist <- data %>%
      filter(country_fa == country_name) %>%
      mutate(cluster_70_names = str_to_title(cluster_70_names)) %>%
      group_by(cluster_70_names) %>%
      count() %>%
      ungroup() %>%
      mutate(rel_freq = n / sum(n)) %>%
      ggplot() +
      geom_bar(aes(x = reorder(cluster_70_names, rel_freq), rel_freq), stat = 'identity',
               fill = 'steelblue') +
      ylab(paste0('Relative Frequency of Clusters')) +
      ggtitle(country_name) +
      xlab(NULL) +
      theme_tufte() +
      theme(
        plot.title = element_text(hjust = .5, size = 20),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    
    
    regional_dist
    
    
  })
  
  output$top_5_clusters <- renderPlot({
    
    country_name <- input$country
    
    
    top_5 <- data %>%
      filter(country_fa == country_name) %>%
      group_by(cluster_70_names) %>%
      count() %>%
      arrange(-n) %>%
      ungroup() %>%
      top_n(5) %>%
      select(cluster_70_names)
    
    
    p <- data %>%
      filter(cluster_70_names %in% top_5$cluster_70_names) %>%
      group_by(PY, cluster_70_names) %>%
      count() %>%
      group_by(cluster_70_names) %>%
      mutate(cumulative = n) %>%
      ggplot() +
      geom_line(aes(PY, cumulative, color = factor(cluster_70_names))) +
      theme_tufte() +
      ylab("Number of Articles") +
      ggtitle(paste0('Top 5 Cluster Over Time; ', country_name)) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank())
    p
    
    
    
  })
  
  
  
  output$top_collab <- renderTable({
    
  country_name <- tolower(input$country)
  
  
  countries_net %>%
    activate(nodes) %>%
    as_data_frame() %>%
    mutate(has_country = ifelse(from == country_name | to == country_name, 1, 0)) %>%
    filter(has_country == 1) %>%
    mutate(from_new = ifelse(to == country_name, to, from),
           to_new = ifelse(to == country_name, from, to)) %>%
    group_by(to_new) %>%
    count() %>%
    ungroup() %>%
    mutate(n_total = sum(n),
           percent_collab = (n/n_total) * 100) %>%
    arrange(desc(percent_collab)) %>%
    top_n(10)  %>%
    select(-n, n_total) %>%
    transmute(`Collaboration With` = to_new, `Percent of Collaboration` = percent_collab) %>%
    mutate(`Collaboration With` = ifelse(`Collaboration With` == 'usa' | `Collaboration With` == 'uk',
                                         toupper(`Collaboration With`), str_to_title(`Collaboration With`)),
           `Percent of Collaboration` = paste0(round(`Percent of Collaboration`, 2), "%"))
  
  
  })
  
  
  
}

shinyApp(ui = ui, server = server)

