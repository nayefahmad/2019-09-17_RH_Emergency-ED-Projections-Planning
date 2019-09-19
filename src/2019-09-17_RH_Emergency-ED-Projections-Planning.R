
#'--- 
#' title: "RHS ED projections"
#' author: "Nayef Ahmad"
#' date: "2019-07-18"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 

#+ lib, include = FALSE 
# 0) libraries: --------------
library(tidyverse)
library(denodoExtractor)
library(DT)

# setup_denodo()
cnx <- DBI::dbConnect(odbc::odbc(), dsn = "cnx_SPDBSCSTA001")

ed_mart <- dplyr::tbl(cnx, dbplyr::in_schema("EDMart.dbo", 
                                             "[vwEDVisitIdentifiedRegional]"))

people_2018 <- dplyr::tbl(cnx, dbplyr::in_schema("DSSI.dbo", 
                                                 "[PEOPLE2018Complete]"))

# functions: 
source(here::here("src", 
                  "plot-pop-vs-year-for-pop-segment_function.R"))

# matching age_group labels: 
df0.age_group_labels <- 
  read_csv(here::here("data", 
                      "2019-09-19_matching-age-group-labels-across-ED-and-population-data.csv"))



#' ## ED visits data 
#+ data 
# 1) ED visits data : ----------------
site <- "RHS"

df1.ed_visits_annual <- 
  ed_mart %>% 
  filter(FacilityShortName == site) %>% 
  select(StartDate, 
         TriageAcuityDescription, 
         Age, 
         PatientID) %>% 
  collect() %>% 
  
  mutate(year = lubridate::year(StartDate), 
         age_group = cut(Age, c(-1, 0, seq(4, 89, 5), 200)),  # 5-yr age buckets
         TriageAcuityDescription = as.factor(TriageAcuityDescription)) %>%
  
  # remove NA and "Invalid":  
  filter(!is.na(Age),
         !TriageAcuityDescription %in% c("Invalid", "8 - Disaster Acuity Only" )) %>% 
  mutate(TriageAcuityDescription = fct_drop(TriageAcuityDescription)) %>% 
  

  # group to year-ctas-age_group level: 
  count(year,
        TriageAcuityDescription, 
        Age, 
        age_group) %>%
  
  rename(ctas = TriageAcuityDescription,
         ed_visits = n)
  

# result: 
# str(df1.ed_visits_annual)
# summary(df1.ed_visits_annual)

#' Note that we remove cases where `Age` = NA (25 rows), and where `age_group` =
#' "Invalid" (255 rows)
#' 
#' We also remove `ctas` levels other than 1 to 5. 

df1.ed_visits_annual %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#' 
#' ### Exploratory plots - RHS ED visits 
# > Exploratory plots - RHS ED visits : -------------

# Annual ED Visits by Calendar Year, broken out by CTAS 
df1.ed_visits_annual %>% 
  group_by(year, 
           ctas) %>% 
  summarise(ed_visits = sum(ed_visits)) %>% 
  
  filter(year != "2019") %>% 
  ggplot(aes(x = year, 
             y = ed_visits, 
             group = ctas, 
             col = ctas)) + 
  geom_line(alpha = 0.5) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  labs(title = sprintf("%s Annual ED Visits by Calendar Year", 
                       site)) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# Annual ED Visits by Calendar Year
df1.ed_visits_annual %>% 
  filter(year != "2019") %>% 
  group_by(year) %>% 
  summarise(ed_visits = sum(ed_visits)) %>% 
  
  ggplot(aes(x = year, 
             y = ed_visits)) + 
  geom_line() + 
  geom_point() + 
  labs(title = sprintf("%s Annual ED Visits by Calendar Year", 
                       site)) +  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


#' Looks like it would make sense to look at historical data from 2010 onwards
#' 







#' ## BC Population data
#' 
#' See file *`r here::here("src", "src/2019-09-17_rh_historical-ed-visits-data.sql")`*
#'  
#' Peter is using total BC population as the predictor, *not* limiting to
#' `HSDAName` = "Richmond". That's what I'll do as well.

#**********************************************
# 2) BC Population data: --------------

df2.bc_population <- 
  people_2018 %>% 
  filter(Year >= "2010", 
         Year <= "2036") %>% 
  select(Year, 
         AgeGroup, 
         Population) %>% 
  collect() %>% 
  group_by(Year, 
           AgeGroup) %>% 
  summarise(pop = sum(Population)) %>% 
  rename(year = Year, 
         age_group_pop = AgeGroup) %>% 
  as_tibble()

# str(df2.bc_population)

df2.bc_population %>% 
  datatable(extensions = 'Buttons',
          options = list(dom = 'Bfrtip', 
                         buttons = c('excel', "csv")))





#' ### Population growth by age group
#' 
#+ pop-by-age
# 3) Population growth by age group:-------------

# create nested df: 
df3.pop_nested <- 
  df2.bc_population %>% 
  group_by(age_group_pop) %>% 
  nest()


# map plotting function across all age groups: 
df3.pop_nested <- 
  df3.pop_nested %>% 
  mutate(age_group_growth = map2(data, 
                                 age_group_pop, 
                                 plot_trend))

#' Too many graphs to show here. Look in *`r here::here("results", "dst")`*
#' 
#' Here's just one example: 
#' 

# example: 
df3.pop_nested$age_group_growth[[sample(1:20, 1)]]

# save output: 
# pdf(here::here("results", 
#                "dst", 
#                "2019-09-18_bc_pop-growth-by-age-group.pdf"))
# df3.pop_nested$age_group_growth
# dev.off()







#' ## Join ED data with population data 
#' 
# 4) Join ED data with population data -----------------

#' First match the `age_group` labels: 
#' 

df1.1.ed_visits <- 
  df1.ed_visits_annual %>% 
  left_join(df0.age_group_labels) 

#' Then join ED with population data: 
#' 

df4.ed_and_pop_data <- 
  df1.1.ed_visits %>% 
  filter(year >= "2010", 
         year != "2019") %>% 
  group_by(year, 
           age_group_pop, 
           ctas) %>% 
  summarise(ed_visits = sum(ed_visits)) %>% 
  
  select(year, 
         age_group_pop, 
         ctas, 
         ed_visits) %>% 
  
  left_join(df2.bc_population) %>% 
  
  arrange(age_group_pop,
          year, 
          ctas) %>% 
  ungroup()

# view result: 
df4.ed_and_pop_data %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


# str(df4.ed_and_pop_data)
# summary(df4.ed_and_pop_data)



#' ### Plots - joined dataset
# > Plots - joined dataset: --------------

#' Distribution of num visit by age segment, in 2018
#' 

# ED visits in 2018, by age group
df4.ed_and_pop_data %>% 
  filter(year == "2018") %>% 
  
  ggplot(aes(x = as.factor(age_group_pop), 
             y = ed_visits)) + 
  geom_boxplot() + 
  facet_wrap(~ctas) + 
  labs(title = sprintf("%s ED visits in 2018, by age group", 
                       site), 
       subtitle = "todo: \"5-9\" age group is in the wrong place") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

# dist of population by age: 
df4.ed_and_pop_data %>% 
  filter(year == "2018") %>% 
  select(age_group_pop, 
         pop) %>% 
  distinct() %>% 
  
  ggplot(aes(x = as.factor(age_group_pop), 
             y = pop)) + 
  geom_col(fill = "steelblue4") + 
  scale_y_continuous(limits = c(0, 500000), 
                     breaks = seq(0, 500000, 50000), 
                     labels = sprintf("%i K", seq(0, 500, 50))) + 
  labs(title = "\"Shape\" of the BC population in 2018, by age group", 
       subtitle = "todo: \"5-9\" age group is in the wrong place") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


#' High-level view of `ed_visits` vs `pop`, across all segments: 
#' 

df4.ed_and_pop_data %>% 
  filter(year == "2018") %>% 
  ggplot(aes(x = pop, 
             y = ed_visits, 
             )) + 
  geom_jitter()
  
#' Not sure that there's much to take away from that.   
  
  




# todo: recode year as int - years from 2010
  
  
  

#'
#' ## Appendix 
# Appendix -----------

# write outputs: 