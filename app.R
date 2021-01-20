### Auto Install Required Packages ###

# list.of.packages <- c("tidyverse", "ggthemes", "tidygraph",
#                       "shinyWidgets", "DescTools",
#                       "scales", "shiny", "igraph", "ggthemes",
#                       "widyr", "visNetwork", "RColorBrewer", "tidytext", "countrycode")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)>0) {install.packages(new.packages)}


require(tidyverse)
require(ggthemes)
require(tidygraph)
library(shinyWidgets)
require(scales)
library(igraph)
library(ggraph)
require(widyr)
require(visNetwork)
require(tidytext)
require(RColorBrewer)
require(DescTools)
require(shiny)
require(countrycode)
library(readxl)
library(cowplot)



world_regions <- tribble(
  ~small, ~large,
  "Australia and New Zealand", "Oceania",
  "Latin America and the Caribbean", 'South America',
  "Central Asia", "Asia",
  "Eastern Africa", "Africa",
  "Eastern Asia", "Asia", 
  "Eastern Europe", 'Europe',
  "Melanesia", "Oceania",
  "Micronesia", "Oceania",
  "Middle Africa",  "Africa",
  "Northern Africa", "Africa",
  "Northern America", "North America",
  "Northern Europe", 'Europe',
  "Polynesia", 'Oceania',
  "Latin America and the Caribbean", "South America",
  "South-eastern Asia", "Asia",
  "Southern Africa", "Africa",
  "Southern Asia", "Asia",
  "Southern Europe", 'Europe',
  "Western Africa", "Africa",
  "Western Asia", "Asia",
  "Western Europe", "Europe",
  "Sub-Saharan Africa", 'Africa'
)

get_region <- function(country) {
  
  require(countrycode)
  out <- ifelse(
    country == 'micronesia', 'Oceania',
    ifelse(country == 'taiwan', 'Asia',
           world_regions$large[match(countrycode(country, origin = 'country.name',
                                                 destination = 'un.regionsub.name'),
                                     world_regions$small)])
  )
  
  return(out)
}



data <- read_csv("shiny_data.gz")
# add EU 
data <- data %>% add_row(country_fa = "Eu")
#
bbnj <- read_csv("states.csv")
concepts_bbnj <- read_csv("concept_count.csv")
cnet <- read_csv("cnet.csv")
cnet <- cnet %>% select(-c("X1"))

research <- read_excel("research.xlsx")

manual <- "This MARIPOLDATA Marine Biodiversity Dashboard 
serves to inform about international marine biodiversity 
research and politics. By selecting the respective tab, 
it is possible to access per country information on about
marine biodiversity research and indicators on its position in the
BBNJ negotiations." 


load("countries_net.R")

countries_net <- countries_net %>% activate(nodes) %>% mutate(region = get_region(name))


# countries_from_network <- countries_net %>% activate(nodes) %>% pull(name)
# regions_from_network <- countries_net %>% activate(nodes) %>% pull(region)
# 
# current_region = regions_from_network[which(countries_from_network == country_name)]


# test comment

default_background_color <- "#f5f5f2"



ui <- fluidPage(
  setBackgroundColor(default_background_color),
  
  
  titlePanel('Marine Biodiversity Country Dashboard'),
  
  
  sidebarPanel(  selectInput(inputId = "country",
                             label = "Choose a Country",
                             selected = "USA",
                             choices = sort(unique(data$country_fa))),
                 tabPanel("Manual", textOutput(outputId = "manual")),
                 
                 
                 
                 checkboxInput("compare_country_check", "Do you want to compare with\n another country?", value = FALSE),
                 uiOutput("compare_country") # checkbox to see if the user wants another country to compare
                 ),
  

  
  ### can you divide this in two main panels "Marine Biodiversity Research Data" & 
  #  "BBNJ Negotiation Data" and then the existing panels as sub-panels?
  mainPanel(
    tabsetPanel(
      
      
      tabPanel("General Information on Research", htmlOutput(outputId = "info1")),
      
      tabPanel("Investment in Research", plotOutput(outputId = "rd_invest")),
      
      tabPanel("Thematic Clusters", plotOutput(outputId = "barchart_clusters"),
               plotOutput(outputId = "barchart_clusters_compare"),
               plotOutput(outputId = "top_5_clusters"),
               plotOutput(outputId = "top_5_clusters_compare")), 
      
      tabPanel("International Collaboration", tableOutput(outputId = "top_collab")), 
      
      tabPanel("Key-words Network", sliderInput(
        inputId = 'n_words',
        label = 'Select the Maximum Number of Keyword Pairs',
        value = 50,
        min = 50,
        max = 750),
        visNetworkOutput("network",
                         height="1000px")),
      
      tabPanel("Concepts from Marine Scientific Research", tableOutput(outputId = "concepts"))
    ),
    tabsetPanel(      
      tabPanel("General Information on BBNJ Negotiations", textOutput(outputId = "info2")),
      
      tabPanel("Concepts in Negotiations", tableOutput(outputId = "concepts2")),
      
      tabPanel("Participants in Negotiations", tableOutput(outputId = "participants")),
      
      #tabPanel("BBNJ Observations Data", tableOutput(outputId = "bbnj")),
      
      tabPanel("BBNJ References to Science", tableOutput(outputId = "scienceref")),
      
      tabPanel("BBNJ Talk Time by Package", plotOutput(outputId = "time")),
      
      tabPanel("Negotiation Reference Network", 
               visNetworkOutput("refnetwork", height = "1000px"))
    )
  )
  
)


server <- function(input, output, session){
  
  
  
  output$compare_country <- 
    renderUI({
      req(input$compare_country_check)
      selectInput(inputId = "compare_country",
                  label = "Choose a Country for Comparison",
                  selected = "USA",
                  choices = sort(unique(data$country_fa)))
    })
  
  
  
  
  output$manual <- renderText({
    
print(manual)
  })
  
  
  output$info1 <- renderUI({
    
    country_name <- str_to_lower(input$country)
    
    intemediate <- research %>% filter(actor == country_name) %>% 
      select(text_science) %>% print(text_science, sep="<br/>")
    
    if (is.na(intemediate$text_science)) {
      print("info")
    } else {print(intemediate$text_science)}
  })
  
  output$info2 <- renderText({
    
    country_name <- str_to_lower(input$country)
    
    intemediate <- research %>% filter(actor == country_name) %>% 
      select(text_bbnj)
    
    if (is.na(intemediate$text_bbnj)) {
      print("info")
    } else {print(intemediate$text_bbnj)}
  })
  
output$rd_invest <- renderPlot({
    
    country_name <- str_to_lower(input$country)
    
    
 total <- bbnj %>% filter(actor == country_name) %>% 
    select( `2015_expenditure_rd`, `2016_expenditure_rd`,
            `2017_expenditure_rd`, `2018_expenditure_rd`,
            `2019_expenditure_rd`) %>% 
    gather(year,rd_expenditure) %>% 
    ggplot() +
    geom_bar(aes(x = year, y = rd_expenditure), stat = 'identity', fill = 'steelblue') +
    ggtitle(country_name) 
  
  
 percapita <- bbnj %>% filter(actor == country_name) %>% 
    select( `2015_expenditure_rd_pc`, `2016_expenditure_rd_pc`,
            `2017_expenditure_rd_pc`, `2018_expenditure_rd_pc`,
            `2019_expenditure_rd_pc`) %>% 
    gather(year,rd_expenditure_percapita) %>% 
    ggplot() +
    geom_bar(aes(x = year, y = rd_expenditure_percapita), stat = 'identity', fill = 'steelblue')+
    ggtitle(country_name) 
 
 plot_grid(total, percapita,labels = c("Total Research Investment", "Per Capita Research Investment"))
  
  })  
  
  
  
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
  
  
  output$barchart_clusters_compare <- renderPlot({
    
    req(input$compare_country_check)
    
    country_name <- input$compare_country
    
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
      ggtitle(paste0(country_name, ' Top 5 Cluster Over Time')) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank()) +
      scale_color_brewer(type = 'qual', palette = 2)
    p
    
    

    
  })
  
  
output$top_5_clusters_compare <- renderPlot({
  
  req(input$compare_country_check)
  
  country_name <- input$compare_country
  
  
  top_5 <- data %>%
    filter(country_fa == country_name) %>%
    group_by(cluster_70_names) %>%
    count() %>%
    arrange(-n) %>%
    ungroup() %>%
    top_n(5) %>%
    select(cluster_70_names)
  
  
  
  p_compare <- data %>%
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
    ggtitle(paste0(country_name, ' Top 5 Cluster Over Time')) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) +
    scale_color_brewer(type = 'qual', palette = 2)
  p_compare
  
  
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
      "Common Heritage of Mankind",
      "Total"
    )
    
    concepts$concepts <- list_concepts
    
    
    
    if (dim(concepts)[2] == 1) {
      print(paste0("No Concepts in ", current_country, "'s ", "Data"))
    } else {concepts %>%
        filter(value != 0) %>%
        transmute(Concepts = concepts, `Times Occuring` = value)}
    
    
  })
  
  
  output$participants <- renderTable({
    country_name <- str_to_lower(input$country)
    
    bbnj %>% filter(actor == country_name) %>% select(participants_BBNJ_igc1, participants_BBNJ_igc2,
                                                      participants_BBNJ_igc3, participants_CBD_cop18) %>%
      transmute(`Participants in BBNJ IGC 1` = participants_BBNJ_igc1,
                `Participants in BBNJ IGC 2` = participants_BBNJ_igc2,
                `Participants in BBNJ IGC 3` = participants_BBNJ_igc3,
                `Participants in CBD COP 2018` = participants_CBD_cop18)
    
  })
  
  
  
  output$concepts2 <- renderTable({
    
    
    country_name <- str_to_lower(input$country)
    
    out_concept_bbnj <- concepts_bbnj %>% select(concept, actor) %>% 
      tidytext::unnest_tokens(actor, actor, token = 'regex', pattern=", ") %>% 
      filter(actor == country_name) %>% 
      group_by(concept) %>% count() 
    
    if (dim(out_concept_bbnj)[1] == 0) {
      print(paste0("No Concepts in ", str_to_title(country_name), "'s ", "Data"))
    } else {out_concept_bbnj %>%
        transmute(Concept = str_to_title(concept),
                  `Times Occuring`= n) %>% 
        ungroup() %>%
        select(-concept)}
    
    
    
  })
  
  
  # output$bbnj <- renderTable({
  #   
  #   country_name <- str_to_lower(input$country)
  #   
  #   bbnj %>% filter(actor == country_name) %>%
  #     select(agg_frq_sci, agg_total_time) %>%
  #     mutate(agg_frq_sci = ifelse(is.na(agg_frq_sci), "No Data Available", as.integer(agg_frq_sci)),
  #            agg_total_time = ifelse(is.na(agg_total_time), "No Data Available",
  #                                    paste0(round(agg_total_time/60, 2), " Minutes"))) %>%
  #     transmute(`References to Science` = agg_frq_sci,
  #               `Total Speaking Time` = agg_total_time)
  #   
  #   
  #   
  # })
  
  output$scienceref <- renderTable({
    country_name <- str_to_lower(input$country)
    
    table.title <- bbnj %>% filter(actor == country_name) %>%
      select(agg_frq_sci) %>%
      mutate(agg_frq_sci = ifelse(is.na(agg_frq_sci), "No Data Available", as.integer(agg_frq_sci)))
    table.title <- table.title$agg_frq_sci

    my_table <- bbnj %>% filter(actor == country_name) %>%
      select(sci_fr_igc1_MGR,
             sci_fr_igc1_ABMT,
             sci_fr_igc1_EIA,
             sci_fr_igc1_CBTT,
             sci_fr_igc1_crosscutting,
        sci_fr_igc2_MGR,
             sci_fr_igc2_ABMT,
             sci_fr_igc2_EIA,
             sci_fr_igc2_CBTT,
             sci_fr_igc2_crosscutting,
             sci_fr_igc3_MGR, 
             sci_fr_igc3_ABMT,
             sci_fr_igc3_EIA,
             sci_fr_igc3_CBTT,
             sci_fr_igc3_crosscutting) %>% 
      transmute(`References to Science IGC 1 MGRs` = sci_fr_igc1_MGR,
                `References to Science IGC 1 ABMTs` = sci_fr_igc1_ABMT,
                `References to Science IGC 1 EIAs` = sci_fr_igc1_EIA,
                `References to Science IGC 1 CBTT` = sci_fr_igc1_CBTT, 
                `References to Science IGC 1 Crosscutting` = sci_fr_igc1_crosscutting,
        `References to Science IGC 2 MGRs` = sci_fr_igc2_MGR,
                `References to Science IGC 2 ABMTs` = sci_fr_igc2_ABMT,
                `References to Science IGC 2 EIAs` = sci_fr_igc2_EIA,
                `References to Science IGC 2 CBTT` = sci_fr_igc2_CBTT, 
                `References to Science IGC 2 Crosscutting` = sci_fr_igc2_crosscutting,
                `References to Science IGC 3 MGRs` = sci_fr_igc3_MGR,
              `References to Science IGC 3 ABMTs` = sci_fr_igc3_ABMT,
              `References to Science IGC 3 EIAs` = sci_fr_igc3_EIA,
              `References to Science IGC 3 CBTT` = sci_fr_igc3_CBTT, 
              `References to Science IGC 3 Crosscutting` = sci_fr_igc3_crosscutting) %>% 
      as.tibble() 
    
    

    pivot_longer(my_table, cols = starts_with("References to Science IGC")) %>%
      rowwise() %>%
      mutate(IGC = paste("IGC", str_split(name, " ")[[1]][5]),
             Package = str_split(name, " ")[[1]][6]) %>%
      pivot_wider(id_cols = IGC,
                  names_from = Package)
    }, caption.placement = "top")
  
  
  output$time <- renderPlot({
    
    country_name <- str_to_lower(input$country)
    
    bbnj_output <- bbnj %>% filter(actor == country_name)
    
    plot.title <- bbnj_output %>% 
      select(agg_total_time) %>%
      mutate(agg_total_time = ifelse(is.na(agg_total_time), "No Data Available",
                                     paste0("Total Speaking Time: ", round(agg_total_time/60, 2), " Minutes")))
    
    
    
    if (dim(bbnj_output)[1] == 0) {
      
      par(mar = c(0,0,0,0),
          bg = default_background_color)
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("No Data Available"), 
           cex = 1.6, col = "black",
           bg = "blue")
      
      
    } else {
      
      bbnj_output %>%
        select(talk_time_MGR, talk_time_ABMT, talk_time_EIA, talk_time_CBTT, talk_time_crosscutting) %>% 
        prop.table() %>% 
        as_tibble() %>%
        transmute(
          MGR  = talk_time_MGR,
          ABMT = talk_time_ABMT,
          EIA  = talk_time_EIA,
          CBTT = talk_time_CBTT,
          Crosscutting = talk_time_crosscutting
        ) %>%
        gather(Package, Time, MGR:Crosscutting) %>% 
        ggplot() + 
        geom_bar(stat = "identity", aes(y = Time, x = Package),  fill = "steelblue") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
        ylab("% of Speaking Time") +
        ggtitle(plot.title) +
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
                                           color = NA))
    }
    
    
  })
  
  
  output$refnetwork <- renderVisNetwork({  
    
    country_name <- str_to_lower(input$country)
    
    
    
    cnet <- as.matrix(cnet)
    
    rownames(cnet) <- NULL
    rownames(cnet) <- colnames(cnet)
    
    
    diag(cnet) <- 0
    
    net <- graph_from_adjacency_matrix(cnet, weighted = TRUE,
                                       mode = "undirected")
    
    degree(net)[degree(net) == 0]
    
    net <- simplify(net, remove.loops = TRUE)
    Isolated = which(degree(net)==0)
    net = delete.vertices(net, Isolated)
    
    
    my_colors <- rep("#4885C1", length(V(net)))
    my_colors[which(V(net)$name == country_name)] <- "#AE3A4E"
    
    V(net)$color <- my_colors
    
    
    data <- toVisNetworkData(net)
    
    plot <- visNetwork(nodes = data$nodes, edges = data$edges, height = "1000px", width = "100%") %>% 
      visIgraphLayout()  
    
    
    visIgraph(net)
  })  
  
  
}

shinyApp(ui = ui, server = server)

