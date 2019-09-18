
#'--- 
#' title: "RHS ED projections"
#' author: "Nayef Ahmad"
#' date: "2019-07-18"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---
#' 

#+ lib, include = FALSE 
library(tidyverse)
library(denodoExtractor)
library(DT)

# setup_denodo()

ed_mart <- dplyr::tbl(cnx, dbplyr::in_schema("EDMart.dbo", 
                                             "[vwEDVisitIdentifiedRegional]"))

#+ data 
# pull rhs ed visit data: ----------------
df1.ed_visits_annual <- 
  vw_eddata %>% 
  filter(facility_name == "Richmond Hospital") %>% 
  select(start_date_calendar_year, 
         first_triage_acuity_desc, 
         patient_id) %>% 
  count(start_date_calendar_year, 
        first_triage_acuity_desc) %>% 
  collect() %>% 
  
  mutate(n = as.integer(n)) %>% 
  rename(year = start_date_calendar_year, 
         ctas = first_triage_acuity_desc, 
         ed_visits = n)

str(df1.ed_visits_annual)

df1.ed_visits_annual %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

df1.ed_visits_annual %>% 
  ggplot(aes(x = year, 
             y = ed_visits)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "RHS Annual ED Visits by Calendar Year") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      

#' Looks like it would make sense to look at historical data from 2010 onwards
