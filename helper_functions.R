# Helper Functions --------------------------------------------------------


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



create_concepts <- function(country_name) {
  data_country <- data %>%
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
  concepts$concepts <- list_concepts
  if (dim(concepts)[2] == 2) {concepts$value <- as.numeric(concepts$value)}
  
  if (dim(concepts)[2] == 1) {
    print(paste0("No Concepts in ", country_name, "'s ", "Data"))
  } else {concepts %>%
      filter(value != 0) %>%
      transmute(Concepts = concepts, `Times Occuring` = value)%>% 
      arrange(desc(`Times Occuring`))
  }
}


regional_dist_plot <- function(country_name) {
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
  return(regional_dist)
}

top_5_clusters_plot <- function(country_name) {
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
  return(p)
}

rnd_plot <- function(country_name) {
  total <- bbnj %>% filter(actor == country_name) %>% 
    select( `2015_expenditure_rd`, `2016_expenditure_rd`,
            `2017_expenditure_rd`, `2018_expenditure_rd`,
            `2019_expenditure_rd`) %>% 
    mutate(`2015` = `2015_expenditure_rd`,
           `2016` = `2016_expenditure_rd`,
           `2017` = `2017_expenditure_rd`,
           `2018` = `2018_expenditure_rd`,
           `2019` = `2019_expenditure_rd`) %>%
    select(!contains("_")) %>%
    gather(year,rd_expenditure) %>% 
    ggplot() +
    geom_bar(aes(x = year, y = rd_expenditure), stat = 'identity', fill = 'steelblue') +
    theme_tufte() +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_blank(),
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
    ylab("R&D Expenditure")
  
  
  percapita <- bbnj %>% filter(actor == country_name) %>% 
    select( `2015_expenditure_rd_pc`, `2016_expenditure_rd_pc`,
            `2017_expenditure_rd_pc`, `2018_expenditure_rd_pc`,
            `2019_expenditure_rd_pc`) %>% 
    mutate(`2015` = `2015_expenditure_rd_pc`,
           `2016` = `2016_expenditure_rd_pc`,
           `2017` = `2017_expenditure_rd_pc`,
           `2018` = `2018_expenditure_rd_pc`,
           `2019` = `2019_expenditure_rd_pc`) %>%
    select(!contains("_")) %>%
    gather(year,rd_expenditure_percapita) %>% 
    ggplot() +
    geom_bar(aes(x = year, y = rd_expenditure_percapita), stat = 'identity', fill = 'steelblue')+
    theme_tufte() +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_blank(),
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
    ylab("R&D Expenditure per Capita")
  
  invest <- plot_grid(total, percapita,labels = c(paste0(str_to_title(country_name)," Total Research Investment"),
                                                  paste0(str_to_title(country_name), " Per Capita Research Investment")))
  return(invest)
}


top_collab_table <- function(country_name) {
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
}


coocurence_network <- function(country_name, n_words) {
  tidy_keywords <- data %>%
    filter(country_fa == country_name) %>%
    dplyr::select(ID, UT) %>%
    unnest_tokens(keyword, ID,
                  token = stringr::str_split, pattern = ";") %>%
    mutate(keyword = StrTrim(keyword)) %>%
    mutate(keyword = str_replace_all(keyword, " - ", "-"))
  word_pairs <- tidy_keywords %>% 
    pairwise_count(keyword, UT, sort = TRUE, upper = FALSE)
  word_pairs <- word_pairs[1:n_words, ]
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
  
  visNetwork(vis_g$nodes, vis_g$edges,
             main = paste(country_name, " Key-word Co-occurence Network")) %>%
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


participants_table <- function(country_name) {
  bbnj %>% filter(actor == country_name) %>% select(participants_BBNJ_igc1, participants_BBNJ_igc2,
                                                    participants_BBNJ_igc3, participants_CBD_cop18) %>%
    transmute(`Participants in BBNJ IGC 1` = participants_BBNJ_igc1,
              `Participants in BBNJ IGC 2` = participants_BBNJ_igc2,
              `Participants in BBNJ IGC 3` = participants_BBNJ_igc3,
              `Participants in CBD COP 2018` = participants_CBD_cop18) %>%
    pivot_longer(cols = starts_with("Participants")) %>%
    transmute(Event = name,
              'Size of Delegation' = value) %>%
    rowwise() %>%
    mutate(Event = paste(str_split(Event, " ")[[1]][3:5], collapse=' '))
}

create_concepts_2 <- function(country_name) {
  out_concept_bbnj <- concepts_bbnj %>% select(concept, actor) %>% 
    tidytext::unnest_tokens(actor, actor, token = 'regex', pattern=", ") %>% 
    filter(actor == country_name) %>% 
    group_by(concept) %>% count() 
  
  if (dim(out_concept_bbnj)[1] == 0) {
    print(paste0("No Concepts in ", str_to_title(country_name), "'s ", "Data"))
  } else {out_concept_bbnj %>%
      transmute(Concept = str_to_title(concept),
                `Times Occuring`= n) %>% 
      ungroup() %>% arrange(desc(`Times Occuring`)) %>%
      select(-concept)}
}


science_ref_first <- function(country_name) {
  bbnj_output <- bbnj %>% filter(actor == country_name)
  table.title <- bbnj %>% filter(actor == country_name) %>%
    select(agg_frq_sci) %>%
    mutate(agg_frq_sci = ifelse(is.na(agg_frq_sci), "No Data Available", as.integer(agg_frq_sci)))
  table.title <- table.title$agg_frq_sci
  first_table <-  if (is.na(bbnj_output$total_time)) {
    paste0("No data available for this country, please look at its alliance: ", str_to_upper(bbnj_output$member_alliance))
  } else {
    my_table_1 <- bbnj_output %>%
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
    
    
    pivot_longer(my_table_1, cols = starts_with("References to Science IGC")) %>%
      rowwise() %>%
      mutate(IGC = paste("IGC", str_split(name, " ")[[1]][5]),
             Package = str_split(name, " ")[[1]][6]) %>%
      pivot_wider(id_cols = IGC,
                  names_from = Package)
  }
}

science_ref_second <- function(country_name) {
  bbnj_output <- bbnj %>% filter(actor == country_name)
  bbnj_output$member_alliance <- str_to_lower(bbnj_output$member_alliance)
  bbnj_output_2 <- filter(bbnj, actor == bbnj_output$member_alliance)
  bbnj_output$alliance == bbnj_output$actor
  second_table <- if (bbnj_output$alliance == bbnj_output$actor) {
    paste0("This country has no alliance data") 
  } else { 
    bbnj_output_2 %>%
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
      transmute(`References to Science IGC 1 MGRs` = round(sci_fr_igc1_MGR,0),
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
      as.tibble() %>%
      pivot_longer(cols = starts_with("References to Science IGC")) %>%
      rowwise() %>%
      mutate(IGC = paste("IGC", str_split(name, " ")[[1]][5]),
             Package = str_split(name, " ")[[1]][6]) %>%
      pivot_wider(id_cols = IGC,
                  names_from = Package)
  }
  print(second_table)
}

plot_time <- function(country_name) {
  bbnj_output <- bbnj %>% filter(actor == country_name)
  
  plot.title_1 <- bbnj_output %>% 
    select(total_time) %>%
    mutate(total_time = ifelse(is.na(total_time), "",
                               paste0("Total Speaking Time: ", round(total_time/60, 2), " Minutes")))
  
  
  
  if (is.na(bbnj_output$total_time) | bbnj_output$total_time == 0) {
    
    first <- ggplot() +
      theme_void() +
      theme(
        plot.background = element_rect(fill = default_background_color,
                                       color = NA)
      )+
      geom_text(aes(0,0,label=paste0("No data available for this country,\nplease look at its alliance: ", str_to_upper(bbnj_output$member_alliance))),
                size = 6) +
      xlab(NULL)
    
  } else {
    first <- bbnj_output %>%
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
      ggtitle(plot.title_1) +
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
  bbnj_output$member_alliance <- str_to_lower(bbnj_output$member_alliance)
  bbnj_output_2 <- filter(bbnj, actor == bbnj_output$member_alliance)
  
  plot.title_2 <- bbnj_output_2 %>% 
    select(agg_total_time) %>%
    mutate(agg_total_time = ifelse(is.na(agg_total_time), "No Data Available",
                                   paste0("Total Speaking Time ", bbnj_output$member_alliance,": ", round(agg_total_time/60, 2), " Minutes")))
  if (bbnj_output$alliance == bbnj_output$actor | dim(bbnj_output_2)[1] == 0) {
    second <- ggplot() +
      theme_void() +
      theme(
        plot.background = element_rect(fill = default_background_color,
                                       color = NA)
      )+
      geom_text(aes(0,0,label='This country has no alliance data'),
                size = 6) +
      xlab(NULL)
  } else {
    second <- bbnj_output_2 %>%
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
      ggtitle(plot.title_2) +
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
  plot_grid(first, second)
}

reference_network <- function(country_name) {
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
}