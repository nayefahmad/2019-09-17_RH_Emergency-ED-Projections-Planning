
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
# libraries: --------------
library(tidyverse)
library(denodoExtractor)
library(DT)

# setup_denodo()
cnx <- DBI::dbConnect(odbc::odbc(), dsn = "cnx_SPDBSCSTA001")

ed_mart <- dplyr::tbl(cnx, dbplyr::in_schema("EDMart.dbo", 
                                             "[vwEDVisitIdentifiedRegional]"))

people_2018 <- dplyr::tbl(cnx, dbplyr::in_schema("DSSI.dbo", 
                                                 "[PEOPLE2018Complete]"))
#' ## ED visits data 
#+ data 
# pull rhs ed visit data: ----------------
site <- "RHS"

df1.ed_visits_annual <- 
  ed_mart %>% 
  filter(FacilityShortName == site) %>% 
  select(StartDate, 
         TriageAcuityDescription, 
         PatientID) %>% 
  collect() %>% 
  
  mutate(year = lubridate::year(StartDate)) %>% 
  count(year, 
        TriageAcuityDescription) %>% 
  
  rename(ctas = TriageAcuityDescription, 
         ed_visits = n)

# str(df1.ed_visits_annual)

df1.ed_visits_annual %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

df1.ed_visits_annual %>% 
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
# pull Richmond population: --------------

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
         age_group = AgeGroup) %>% 
  as_tibble()

# str(df2.bc_population)

df2.bc_population %>% 
  datatable(extensions = 'Buttons',
          options = list(dom = 'Bfrtip', 
                         buttons = c('excel', "csv")))

#' ### Population growth by age group
#' 
#' Too many graphs to show here. Look in *`r here::here("results", "dst")`*

#+ pop-by-age
# pop by age group:-------------

# create nested df: 
df3.pop_nested <- 
  df2.bc_population %>% 
  group_by(age_group) %>% 
  nest()

# plotting function: 
plot_trend <- function(df, 
                       subset = NULL){
  # arguments: 
  # df: df with cols: year, pop
  # subset: optional, used to specify which subset of population we're 
  #   focusing on. E.g. "65-69 years old"
  
  # returns ggplot with trend line
  
  df %>% 
    ggplot(aes(x = year, 
               y = pop)) + 
    geom_point() + 
    geom_line() + 
    scale_y_continuous(limits = c(0, 500000), 
                       breaks = seq(0, 500000, 50000), 
                       labels = sprintf("%i K", seq(0, 500, 50))) + 
    labs(title = sprintf("%s : Population growth for all of BC", 
                         subset), 
         caption = "Data source: PEOPLE 2018, BC Stats \nRetrieved from: [DSSI].[dbo].[PEOPLE2018Complete]", 
         subtitle = "2010 to 2036") + 
    theme_light() +
    theme(panel.grid.minor = element_line(colour = "grey95"), 
          panel.grid.major = element_line(colour = "grey95"))
        
  
}


# test function: 
# First run the following script until definition of `df3.pop_nested`: 
# "src/2019-09-17_RH_Emergency-ED-Projections-Planning.R"
# df3.pop_nested$data[[1]] %>% plot_trend()

# map plotting function across all age groups: 
df3.pop_nested <- 
  df3.pop_nested %>% 
  mutate(age_group_growth = map2(data, 
                                 age_group, 
                                 plot_trend))

# save output: 
# pdf(here::here("results", 
#                "dst", 
#                "2019-09-18_bc_pop-growth-by-age-group.pdf"))
# df3.pop_nested$age_group_growth
# dev.off()
