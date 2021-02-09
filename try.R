my_data_country <- data %>%
  group_by(country_fa) %>%
  select(starts_with("search_")) %>%
  summarise_all(funs('sum' = sum)) %>%
  mutate(sumVar = rowSums(select(., starts_with("search_")))) %>%
  filter(sumVar != 0,
         !is.na(country_fa))



concepts_t <- my_data_country %>%
  filter(country_fa == "USA") %>% t()
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
concepts$value <- as.integer(concepts$value)

class(concepts$value)
      
if (dim(concepts)[2] == 1) {
  print(paste0("No Concepts in ", "current_country", "'s ", "Data"))
} else {concepts %>%
    filter(value != 0) %>% 
    transmute(Concepts = concepts, `Times Occuring` = value) %>% 
    arrange(desc(`Times Occuring`))}


