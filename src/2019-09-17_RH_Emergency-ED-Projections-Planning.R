
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



#**********************************************
# pull Richmond population: --------------






