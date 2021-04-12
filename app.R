#####################################
### Petro Tolochko & Arne Langlet ###
###        ERC Maripoldata        ###
#####################################

### Auto Install Required Packages ###
# list.of.packages <- c("tidyverse", "ggthemes", "tidygraph",
#                       "shinyWidgets", "DescTools",
#                       "scales", "shiny", "igraph", "ggthemes",
#                       "widyr", "visNetwork", "RColorBrewer", "tidytext", "countrycode", "shinythemes)
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
require(gridExtra)
require(shinythemes)
require(kableExtra)
library(png)

default_background_color <- "#f5f5f2"

# Load Helper Functions ---------------------------------------------------
# source("helper_functions.R")
source("helper_functions.R", local = TRUE)$value

# load alliance data
source("alliance.R", local = TRUE)$value


# Reading Data #------------------------------------------------------------
data <- read_csv("shiny_data.gz")
bbnj <- read_csv("states.csv")
concepts_bbnj <- read_csv("concept_count.csv")
cnet <- read_csv("cnet.csv")
cnet <- cnet %>% select(-c("X1"))
research <- read_excel("research.xlsx")
list_concepts <- read_csv("list_concepts.csv", col_names = F) %>%
  pull(X1)
world_regions <- read_csv("world_regions.csv")
load("countries_net.R")
countries_net <- countries_net %>% activate(nodes) %>% mutate(region = get_region(name))

bib_meth <- read_lines("methods_bibliometry.txt")
bib_meth <- paste(bib_meth, collapse = " ")

ethno_meth <- read_lines("methods_ethno.txt")
ethno_meth <- paste(ethno_meth, collapse = " ")

#----------------------------------------------------

manual <- "This MARIPOLDATA Marine Biodiversity Dashboard 
serves to inform about international marine biodiversity 
research and politics. By selecting the respective tab, 
it is possible to access per country information on about
marine biodiversity research and indicators on its position in the
BBNJ negotiations." 

erc <- "This MARIPOLDATA Marine Biodiversity Dashboard is part of the MARIPOLDATA project that has 
received funding from the European Research Council (ERC) under the European Union's Horizon
2020 research and innovation programme (grant agreement No 804599 - MARIPOLDATA - ERC-2018-STG)."

#----------------------------------------------------
### Renaming some incosistently named countries in the bbnj dataset ###
### Could require more manual inspection                            ###
bbnj$actor[which(bbnj$actor == "russian federation")] <- "russia"
bbnj$actor[which(bbnj$actor == "united kingdom")] <- "uk"
bbnj$actor[which(bbnj$actor == "viet nam")] <- "vietnam"
bbnj$actor[which(bbnj$actor == "syrian arabic republic")] <- "syria"
bbnj$actor[which(bbnj$actor == "republic of korea")] <- "south korea"
bbnj$actor[which(bbnj$actor == "brunei darussalam")] <- "brunei"


bbnj$alliance[which(bbnj$alliance == "russian federation")] <- "russia"
bbnj$alliance[which(bbnj$alliance == "viet nam")] <- "vietnam"
bbnj$alliance[which(bbnj$alliance == "syrian arabic republic")] <- "syria"
bbnj$alliance[which(bbnj$alliance == "republic of korea")] <- "south korea"
bbnj$alliance[which(bbnj$alliance == "brunei darussalam")] <- "brunei"




# add Countries (entities) that do not exist in bibliometric data 
data <- data %>% add_row(country_fa = "Eu")
data <- data %>% add_row(country_fa = "Holy See")
data <- data %>% add_row(country_fa = "All")
data <- data %>% add_row(country_fa = "Cooks Island")
data <- data %>% add_row(country_fa = "El Salvador")
data <- data %>% add_row(country_fa = "Dominican Republic")
data <- data %>% add_row(country_fa = "Grenada")



### agg_total_time is not present for some aliance (NA), therefore there is a problem
### for the naming of the second plot (plot.title_2)
### THIS IS PROBABLY WRONG!!! SHOULD BE DEALT WITH PROPERLY IN THE FUTURE ###

bbnj <- bbnj %>%
  mutate(agg_total_time = talk_time_MGR +
           talk_time_ABMT +
           talk_time_EIA +
           talk_time_CBTT +
           talk_time_crosscutting)

# Page Layout #-------------------------------------------------------------
ui <- fluidPage(
  setBackgroundColor(default_background_color),
  sidebarLayout(
    sidebarPanel(
      tags$a(href="https://www.maripoldata.eu",
             tags$img(src='maripol.png', height='120', width='150')),
      tags$a(href="https://politikwissenschaft.univie.ac.at/",
             tags$img(src='logo_P.jpg', height='120', width='340')),
      tags$a(href="https://erc.europa.eu/",
             tags$img(src='Logo_E.png', height='120', width='260')),
      titlePanel('Marine Biodiversity Country Dashboard'),
      tags$a(href="https://www.un.org/bbnj/",
             "Link to the Biodiversity Beyond National Jurisdiction (BBNJ) negotiations."),
      tabPanel("Manual",
               textOutput(outputId = "manual")),
      selectInput(inputId = "country",
                  label = "Choose a Country",
                  selected = "all",
                  choices = c("All", sort(unique(data$country_fa)))),
      checkboxInput("compare_country_check", "Do you want to compare with\n another country?", value = FALSE),
      uiOutput("compare_country"), # checkbox to see if the user wants another country to compare
      tabPanel("ERC",
               textOutput(outputId = "erc")),
    ),
    mainPanel(navbarPage(title = "",
                         
                         tabPanel(h4("Scientometric Data"),
                                  tabsetPanel(
                                    tabPanel("General Information on Ocean Science Marine Biodiversity Research",
                                             htmlOutput(outputId = "info1")),
                                    tabPanel("Research Investments",
                                             plotOutput(outputId = "rd_invest")),
                                    tabPanel("Thematic Clusters",
                                             plotOutput(outputId = "barchart_clusters"),
                                             plotOutput(outputId = "top_5_clusters")),
                                    tabPanel("International Collaboration",
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           tableOutput("top_collab"),
                                                           tableOutput("top_collab_compare"))
                                             )),
                                    tabPanel("Key-words Network", sliderInput(
                                      inputId = 'n_words',
                                      label = 'Select the Maximum Number of Keyword Pairs',
                                      value = 50,
                                      min = 50,
                                      max = 750),
                                      visNetworkOutput("network",
                                                       height="1000px"),
                                      visNetworkOutput('network_compare',
                                                       height="1000px")),
                                    tabPanel("Concepts used in Scientific Research",
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           tableOutput("concepts"),
                                                           tableOutput("concepts_compare"))
                                             )),
                                    tabPanel("Methodology",
                                             htmlOutput("methods_bibliometry"))
                                  )),
                         tabPanel(h4("Ethnographic Data"),
                                  tabsetPanel(
                                    tabPanel("General Information on BBNJ Negotiations",
                                             htmlOutput(outputId = "info2")),
                                    tabPanel("Key Concepts in Statements",
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           tableOutput("concepts2"),
                                                           tableOutput("concepts2_compare"))
                                             ),
                                            fluidRow(
                                              splitLayout(cellWidths = c("50%", "50%"),
                                                            tableOutput("concepts2_alliance"),
                                                            tableOutput("concepts2_alliance_compare"))   
                                             )),
                                    tabPanel("Participants",
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           tableOutput("participants"),
                                                           tableOutput("participants_compare"))
                                             )),
                                    tabPanel("References to Science",
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           tableOutput("scienceref"),
                                                           tableOutput("scienceref_compare"))
                                             ),
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           tableOutput("scienceref_second"),
                                                           tableOutput("scienceref_second_compare"))
                                             )),
                                    tabPanel("Talk Time",
                                             plotOutput(outputId = "time")),
                                    tabPanel("Reference Network",
                                             visNetworkOutput("refnetwork", height = "1000px")),
                                    tabPanel("Methodology",
                                             htmlOutput("methods_ethnography"))
                                  ))
    ))
  )
)


server <- function(input, output, session){
  
  # Add a checkbox for a country comparison
  output$compare_country <- renderUI({
    req(input$compare_country_check)
    selectInput(inputId = "compare_country",
                label = "Choose a Country for Comparison",
                selected = "all",
                choices = c("All", sort(unique(data$country_fa))))
  })
  
  
  ### Reactive Environment For Plots
  ### Each plot is generated by a function from "helper_functions.r" source file
  ### Each function is called twice: 1st time for chosen country, 2nd time for the comparison country
  
  # Regional Distribution of Clusters
  regional_dist <- reactive({
    country_name <- input$country
    regional_dist_plot(country_name)
  })
  
  regional_dist_compare <- reactive({
    if (!input$compare_country_check) return(NULL)
    country_name <- input$compare_country
    regional_dist_plot(country_name)
  })
  
  #----------------------------------------------------
  # Line Chart for Overtime Cluster Plot
  top_5_clusters <- reactive({
    country_name <- input$country
    top_5_clusters_plot(country_name)
  })
  
  top_5_clusters_compare <- reactive({
    if (!input$compare_country_check) return(NULL)
    country_name <- input$compare_country
    top_5_clusters_plot(country_name)
  })
  
  #----------------------------------------------------
  # Research and Development Barchart
  rd_invest <- reactive({
    country_name <- str_to_lower(input$country)
    rnd_plot(country_name)
  })
  
  rd_invest_compare <- reactive({
    if (!input$compare_country_check) return(NULL)
    country_name <- str_to_lower(input$compare_country)
    rnd_plot(country_name)
  }) 
  
  #----------------------------------------------------
  # Speaking Time Barchart
  time_plots <- reactive({
    country_name <- str_to_lower(input$country)
    plot_time(country_name)
  })
  
  time_plots_compare <- reactive({
    if (!input$compare_country_check) return(NULL)
    country_name <- str_to_lower(input$compare_country)
    plot_time(country_name)
  })
  
  
  # Output #------------------------------------------------------------------
  
  # Plots and tables in the output object are also generated (for the most part)
  # from the helper_functions.r source file
  #
  # Each function is called twice -- for first and comparison countries
  
  # Bibliometric Methodology
  output$methods_bibliometry <-renderUI({
    HTML(bib_meth)
  })
  
  # Ethongraphic Methodology
  output$methods_ethnography <-renderText({
    HTML(ethno_meth)
  })
  
  # Dashboard General Description
  output$manual <- renderText({
    HTML(manual)
  })
  
  # ERC Header
  output$erc <- renderText({
    HTML(erc)
  })
  
  
  # Country description 1
  output$info1 <- renderUI({
    country_name <- str_to_lower(input$country)
    intemediate <- research %>% filter(actor == country_name) %>% 
      select(text_science) %>% print(text_science, sep="<br/>")
    if (length(intemediate$text_science) == 0) {
      print("For this country we do not have specific information yet")
    } else {HTML(intemediate$text_science)}
  })
  
  
  # Country description 2
  output$info2 <- renderUI({
    country_name <- str_to_lower(input$country)
    intemediate <- research %>% filter(actor == country_name) %>% 
      select(text_bbnj) %>% print(text_bbnj, sep="<br/>")
    if (length(intemediate$text_bbnj) == 0) {
      print("For this country we do not have specific information yet")
    } else {HTML(intemediate$text_bbnj)}
  })
  
  
  ### Output generated from the reactive environment ###
  
  # Research and Development Output object
  output$rd_invest <- renderPlot({
    ptlist <- list(rd_invest(),rd_invest_compare())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    grid.arrange(grobs=ptlist,nrow=length(ptlist))
  })
  
  #----------------------------------------------------
  # Barcharts for Cluster Distribution
  output$barchart_clusters <- renderPlot({
    ptlist <- list(regional_dist(),regional_dist_compare())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
  
  #----------------------------------------------------
  # Linecharts for Cluster Distribution
  output$top_5_clusters <- renderPlot({
    ptlist <- list(top_5_clusters(), top_5_clusters_compare())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
  
  #----------------------------------------------------
  # Top Collaboration Partner Table(s)
  output$top_collab <- renderTable({
    country_name <- tolower(input$country)
    top_collab_table(country_name)
  })
  
  output$top_collab_compare <- renderTable({
    req(input$compare_country_check)
    country_name <- tolower(input$compare_country)
    top_collab_table(country_name)
  })
  
  #----------------------------------------------------
  # Keyword co-occurence network(s)
  output$network <- renderVisNetwork({
    country_name <- input$country
    coocurence_network(country_name, input$n_words)
  })
  
  output$network_compare <- renderVisNetwork({
    req(input$compare_country_check)
    country_name <- input$compare_country
    coocurence_network(country_name, input$n_words)
  })
  
  #----------------------------------------------------
  # Table for concepts from Marine Scientific Research
  output$concepts <- renderTable({
    country_name <- input$country
    concepts <- create_concepts(country_name)
  }, digits = 0)
  
  output$concepts_compare <- renderTable({
    req(input$compare_country_check)
    country_name <- input$compare_country
    concepts <- create_concepts(country_name)
  }, digits = 0)
  
  #----------------------------------------------------
  # Table for BBNJ participants
  output$participants <- renderTable({
        country_name <- str_to_lower(input$country)
        participants_table(country_name)
      }, digits = 0)
  
  output$participants_compare <- renderTable({
    req(input$compare_country_check)
    country_name <- str_to_lower(input$compare_country)
    participants_table(country_name)
  }, digits = 0)
  
  #----------------------------------------------------
  # Table for concepts from BBNJ
  output$concepts2 <- renderTable({
    country_name <- str_to_lower(input$country)
    create_concepts_2(country_name)
  }, digits = 0)
  
  output$concepts2_alliance <- renderTable({
    country_name <- str_to_lower(input$country)
    create_concepts_2_alliance(country_name)
  }, digits = 0)
  
  output$concepts2_compare <- renderTable({
    req(input$compare_country_check)
    country_name <- str_to_lower(input$compare_country)
    create_concepts_2(country_name)
  }, digits = 0)
  
  output$concepts2_alliance_compare <- renderTable({
    req(input$compare_country_check)
    country_name <- str_to_lower(input$compare_country)
    create_concepts_2_alliance(country_name)
  }, digits = 0)
  #----------------------------------------------------
  # Tables for references to science from BBNJ data
  output$scienceref <- renderTable({
    country_name <- str_to_lower(input$country)
    science_ref_first(country_name)
  }, digits = 0)
  
  output$scienceref_second <- renderTable({
    country_name <- str_to_lower(input$country)
    science_ref_second(country_name)
  }, digits = 0)
  
  output$scienceref_compare <- renderTable({
    req(input$compare_country_check)
    country_name <- str_to_lower(input$compare_country)
    science_ref_first(country_name)
  }, digits = 0)
  
  output$scienceref_second_compare <- renderTable({
    req(input$compare_country_check)
    country_name <- str_to_lower(input$compare_country)
    science_ref_second(country_name)
  }, digits = 0)
  
  #----------------------------------------------------
  # Plot for time talking about BBNJ Packages 
  output$time <- renderPlot({
    ptlist <- list(time_plots(), time_plots_compare())
    to_delete <- !sapply(ptlist, is.null)
    ptlist <- ptlist[to_delete] 
    grid.arrange(grobs=ptlist, nrow=length(ptlist))
  })
  
  #----------------------------------------------------
  # Reference Network
  output$refnetwork <- renderVisNetwork({  
    country_name <- str_to_lower(input$country)
    reference_network(country_name)
  })
  
  #----------------------------------------------------
  
}


shinyApp(ui = ui, server = server)

