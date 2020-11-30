require(tidyverse)
require(ggthemes)
require(tidygraph)
require(tidytext)

data <- read_csv("wos_data_wit_collab.csv")
# 
# data_abridged <- data[, -c(1:71)]
# UT <- data$UT
# ID <- data$ID
# data_abridged$UT <- UT
# data_abridged$ID <- ID
# data_abridged <- data_abridged[, -c(27:56)]
# 
# write_csv(data_abridged, "shiny_data.csv")

country_name <- "Greece"

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




# Select top 5 most popular clusters --------------------------------------

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



countries_net <- 
  make_net(data$country, data$PY)
countries_net <- countries_net %>% activate(nodes) %>% mutate(region = get_region(name))
countries_net <- countries_net %>% activate(edges) %>% filter(!edge_is_loop())

save(countries_net, file = "countries_net.R")


country <- "united arab emirates"
countries_net %>%
  activate(nodes) %>%
  as_data_frame() %>%
  mutate(has_country = ifelse(from == country | to == country, 1, 0)) %>%
  filter(has_country == 1) %>%
  mutate(from_new = ifelse(to == country, to, from),
         to_new = ifelse(to == country, from, to)) %>%
  group_by(to_new) %>%
  count() %>%
  ungroup() %>%
  mutate(n_total = sum(n),
         percent_collab = (n/n_total) * 100) %>%
  arrange(desc(percent_collab)) %>%
  top_n(10) %>%
  select(-n, n_total) %>%
  transmute(`Collaboration With` = to_new, `Percent of Collaboration` = percent_collab) %>%
  mutate(`Collaboration With` = ifelse(`Collaboration With` == 'usa' | `Collaboration With` == 'uk',
                                       toupper(`Collaboration With`), str_to_title(`Collaboration With`)),
         `Percent of Collaboration` = paste0(round(`Percent of Collaboration`, 2), "%"))
