unique(data$country_fa)

country_name <- "eu"



bbnj %>% filter(actor == country_name) %>%
  select(agg_frq_sci, agg_total_time) %>%
  mutate(agg_frq_sci = ifelse(is.na(agg_frq_sci), "No Data Available", as.integer(agg_frq_sci)),
         agg_total_time = ifelse(is.na(agg_total_time), "No Data Available",
                             paste0(round(agg_total_time/60, 2), " Minutes"))) %>%
  transmute(`References to Science` = agg_frq_sci,
            `Total Speaking Time` = agg_total_time)



bbnj %>% filter(actor == country_name) %>%
  select(sci_fr_igc2_MGR,
         sci_fr_igc2_ABMT,
         sci_fr_igc2_EIA,
         sci_fr_igc2_CBTT,
         sci_fr_igc2_crosscutting,
    sci_fr_igc3_MGR, 
         sci_fr_igc3_ABMT,
         sci_fr_igc3_EIA,
         sci_fr_igc3_CBTT,
         sci_fr_igc3_crosscutting)


