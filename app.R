### Auto Install Required Packages ###

list.of.packages <- c("tidyverse", "ggthemes", "tidygraph",
                      "shinyWidgets", "DescTools",
                      "scales", "shiny", "igraph", "ggthemes",
                      "widyr", "visNetwork", "RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}


require(tidyverse)
require(ggthemes)
require(tidygraph)
library(shinyWidgets)
require(scales)
library(igraph)
library(ggraph)
require(widyr)
require(visNetwork)
require(RColorBrewer)
require(DescTools)

data <- read_csv("shiny_data.gz")
bbnj <- read_csv("bbnj_data.csv")

# test comment
load("countries_net.R")

default_background_color <- "#f5f5f2"



ui <- fluidPage(
  setBackgroundColor(default_background_color),
  
  
  titlePanel('Marine Biodiversity Country Publications Dashboard'),
  
  sidebarPanel(  selectInput(inputId = "country",
                             label = "Choose a Country",
                             selected = "USA",
                             choices = sort(unique(data$country_fa)))),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Thematic Clusters",   plotOutput(outputId = "barchart_clusters"),
               plotOutput(outputId = "top_5_clusters")), 
      
      tabPanel("International Collaboration", tableOutput(outputId = "top_collab")), 
      
      tabPanel("Key-words Network", sliderInput(
        inputId = 'n_words',
        label = 'Select the Maximum Number of Keyword Pairs',
        value = 50,
        min = 50,
        max = 750),
        visNetworkOutput("network",
                         height="1000px")),
      
      tabPanel("Concepts from IR", tableOutput(outputId = "concepts")),
      
      tabPanel("BBNJ Observations Data", tableOutput(outputId = "bbnj"))
    )
  )
  
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
        axis.text.y = element_text(size = 15),
        plot.background = element_rect(fill = default_background_color,
                                       color = NA),
        panel.background = element_rect(fill = default_background_color,
                                        color = NA),
        legend.background = element_rect(fill = default_background_color,
                                         color = NA)) +
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
      mutate(cluster_70_names = str_to_title(cluster_70_names)) %>%
      group_by(PY, cluster_70_names) %>%
      count() %>%
      group_by(cluster_70_names) %>%
      mutate(cumulative = n) %>%
      ggplot() +
      geom_line(aes(PY, cumulative, color = factor(cluster_70_names)),
                size = 2) +
      theme_tufte() +
      theme(
        plot.title = element_text(hjust = .5, size = 20),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 13),
        plot.background = element_rect(fill = default_background_color,
                                       color = NA),
        panel.background = element_rect(fill = default_background_color,
                                        color = NA),
        legend.background = element_rect(fill = default_background_color,
                                         color = NA)) +
      ylab("Number of Articles") +
      ggtitle(paste0('Top 5 Cluster Over Time')) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank()) +
      scale_color_brewer(type = 'qual', palette = 2)
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
  
  
  
  output$network <- renderVisNetwork({
    
    
    country_name <- input$country
    
    
    tidy_keywords <- data %>%
      filter(country_fa == country_name) %>%
      dplyr::select(ID, UT) %>%
      unnest_tokens(keyword, ID,
                    token = stringr::str_split, pattern = ";") %>%
      mutate(keyword = StrTrim(keyword)) %>%
      mutate(keyword = str_replace_all(keyword, " - ", "-"))
    
    
    
    word_pairs <- tidy_keywords %>% 
      pairwise_count(keyword, UT, sort = TRUE, upper = FALSE)
    
    
    word_pairs <- word_pairs[1:input$n_words, ]
    
    
    
    word_pairs <- na.omit(word_pairs)
    
    word_pairs_graph <- word_pairs %>%
      mutate(value = n) %>%
      graph_from_data_frame(directed = FALSE)
    
    
    
    
    word_pairs_graph <- simplify(word_pairs_graph)
    
    c1 = cluster_leading_eigen(word_pairs_graph)
    
    my_colors <- brewer.pal(length(c1), 'RdYlBu')
    
    V(word_pairs_graph)$color <- my_colors[membership(c1)]
    vis_g <- toVisNetworkData(word_pairs_graph)
    
    
    
    vis_g$nodes$font.size <- 20
    
    visNetwork(vis_g$nodes, vis_g$edges) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visEdges(color = list(highlight = 'gold',
                            hover = 'gold'),
               hoverWidth = 3) %>%
      visNodes(color = list(higlight = 'gold',
                            hover = 'gold')) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1,
                                         labelOnly = FALSE, hover = TRUE),
                 nodesIdSelection = TRUE)
    
  }
  )
  
  
  output$concepts <- renderTable({
    
    
    country_name <- input$country
    
    
    my_data_country <- data %>%
      group_by(country_fa) %>%
      select(starts_with("search_")) %>%
      summarise_all(funs('sum' = sum)) %>%
      mutate(sumVar = rowSums(select(., starts_with("search_")))) %>%
      filter(sumVar != 0,
             !is.na(country_fa))
    
    concepts_t <- my_data_country %>%
      filter(country_fa == country_name) %>% t()
    concepts <- concepts_t[-1, ] %>% as_tibble()
    concepts$concepts = rownames(concepts_t)[-1]
    
    list_concepts <- c(
      "In/Ex Situ; In Silico",
      "Biological Productivity",
      "Equitably Managed",
      "Science-based Approach",
      "Ecologically Representative",
      "Common Concern of Humankind",
      "Polluter Pays Principle",
      "Ecosystem Approach / Ecosystem-based Management",
      "Cumulative / Transboundary Impacts",
      "Precautionary Approach / Principle",
      "Adaptive Management",
      "Freedom of the High Seas",
      "Derivatives",
      "Dependency",
      "Naturalness",
      "Traditional Knowledge",
      "Bioprospecting",
      "Good Environmental Governance",
      "Representativeness",
      "Differenciated Protection",
      "Compatibility",
      "Sustainable Developement",
      "Biotechnology",
      "Duty to Protect and Preserve Marine Env.",
      "Integrated (management) Approach",
      "Ecological Processes",
      "Connectivity",
      "Intra- and Intergenerational Equity",
      "Restoration of Integrity of Ecosystems",
      "Ecologically or Biologically Significant Areas",
      "Common Heritage of mankind",
      "Total"
    )
    
    concepts$concepts <- list_concepts
    
    
    
    if (dim(concepts)[2] == 1) {
      print(paste0("No Concepts in ", current_country, "'s ", "Data"))
    } else {concepts %>%
        filter(value != 0) %>%
        transmute(Concepts = concepts, `Times Occuring` = value)}
    
    
  })
  
  
  output$bbnj <- renderTable({
    
    country_name <- str_to_lower(input$country)
    
    bbnj %>% filter(actor == country_name) %>%
      select(frq_sci, total_time) %>%
      mutate(frq_sci = ifelse(is.na(frq_sci), "No Data Available", as.integer(frq_sci)),
             total_time = ifelse(is.na(total_time), "No Data Available",
                                 paste0(total_time, " Minutes"))) %>%
      transmute(`References to Science` = frq_sci,
                `Total Speaking Time` = total_time)
    
    
  })
  
  
}

shinyApp(ui = ui, server = server)

