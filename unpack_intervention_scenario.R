dd <- qread("draggable_data.qs")

dd[2] %>% 
  unlist() %>%
  matrix(ncol=2,byrow = T) %>% 
  as.data.frame() %>%
  setNames(c("year","intervention")) 


# ss <- qread( "myfile.qs")
# ss$intervention_target


r <- qread( "result.qs")

df <-  past_populations

morbidity_list <- c(
  'pad', 'ckd', 'vte', 'diabetes', 'rheumatoid_arthritis', 'copd', 'asthma',
  'depression', 'non_diabetic_hyperglycaemia', 'osteoporosis', 'cancer',
  'osteoarthritis', 'epilepsy', 'hypothyroidism', 'colorectal_cancer',
  'prostate_cancer', 'female_breast_cancer', 'renal_cancer',
  'oesophageal_cancer', 'stomach_cancer', 'osteogastric_cancer',
  'oral_cancer', 'pancreatic_cancer', 'uterine_cancer',
  'blood_multiple_myeloma', 'blood_lymphoma', 'blood_leukaemia',
  'blood_cancer', 'ovarian_cancer', 'lung_cancer', 'stroke',
  'dementia', 'heart_failure', 'atrial_fibrillation', 'hypertension',
  'chronic_kidney_disease'
)

get_time_series <- function(df, morbidity, fun) {
  if (fun == "prevalence") {
    ts <- df[df[[morbidity]]!=0,] %>%
      count(run, year) %>%
      group_by(year) %>%
      summarise(n = mean(n)) %>%
      arrange(year)
  } else {
    ts <- df[df[[morbidity]]!=df[['year']],] %>%
      count(run, year) %>%
      group_by(year) %>%
      summarise(n = mean(n)) %>%
      arrange(year)
  }
  ts$n * model_specification$population$scale_down_factor
}


if (is.null(df) || nrow(df) == 0) return(NULL)

table_data <- lapply(morbidity_list, function(morb) {
  prev <- list(get_time_series(df, morb, "prevalence"))
  inc  <- list(get_time_series(df, morb, "incidence"))
  list(
    Morbidity = gsub("_", " ", tools::toTitleCase(morb)),
    Prevalence = prev,
    Incidence = inc,
    DALY = "-",
    YLL = "-",
    YLD = "-"
  )
})

table_df <- do.call(what = bind_rows, args = lapply(table_data, as_tibble))
# table_df$Prevalence <- lapply(table_data, function(x) x$Prevalence)
# table_df$Incidence <- lapply(table_data, function(x) x$Incidence)


setDT(r)
total_people <- r[
  intervention=='intervention',.(N=sum(intervention_target)) , by = .(run, intervention, year)
][, .(total_people = sum(N)), by = .(intervention, year)
]

r$intervention_target

ts_targeted <- filter(r,intervention_target) %>% 
  count(year,run,,intervention) %>% 
    group_by(year,intervention) %>% 
  summarise(n=mean(n)) %>% 
  ungroup()


ts_targeted %>% 
  filter(n==first(n)) %>% 
  pull(n) 

ts_targeted %>% 
  pull(n) 

dd[2] %>% 
  unlist() %>%
  matrix(ncol=2,byrow = T) %>% 
  as.data.frame() %>%
  setNames(c("year","intervention")) %>% 
  filter(0.05 < abs(intervention-1)) %>% 
  nrow()
  
  ################################################

req(!is.null(simulation_state$results()) & nrow(simulation_state$results())>0)
args <- list( past_populations = as.data.table(r), year_cut_off = NULL)
x <- do.call(qaly_yld_fn, args)
x[disease == "combined_uw", .(total = sum(total_uw))]

tibble::tribble(
        ~year.total_people.fixed_cost.annual_cost.ongoing_patient_cost,
     "<char>        <num>      <int>       <int>                <num>",
  "1:   2021        211.0          0           0                    0",
  "2:   2022        212.0      10000        1000              2120000",
  "3:   2023        213.5          0        1000              2135000",
  "4:   2024        213.5          0        1000              2135000",
  "5:   2025        213.0          0        1000              2130000",
  "6:   2026        214.0          0        1000              2140000",
                       "patient_cost total_cost cumulative_total_cost",
                              "<num>      <num>                 <num>",
                    "1:            0          0                     0",
                    "2:     21200000   23331000              23331000",
                    "3:            0    2136000              25467000",
                    "4:            0    2136000              27603000",
                    "5:            0    2131000              29734000",
                    "6:            0    2141000              31875000"
  )

x[disease == 'combined_uw', ] %>%
  dcast(formula = year  ~ intervention, value.var = 'total_uw', fill = 0L) %>%
  mutate(year = as.character(year)) %>%
  mutate(averted = `non-intervention` - intervention) %>%
  mutate(cumulative_averted = cumsum(averted))
print(x)

x <- x  %>%
  left_join(
    costs()[,.(total_cost = sum(total_cost)), by = .(intervention,year)
    ][,year := as.character(year)
    ] %>%
      dcast(formula = year  ~intervention, value.var = 'total_cost', fill = 0L) %>%
      mutate(savings = `non-intervention` - intervention) %>%
      mutate(cumulative_savings = cumsum(savings)),
    by='year'
  ) %>%
  mutate(cost_per_qaly_gained = cumulative_savings / cumulative_averted
  ) 



r[r[['stroke']]!=0,] %>% 
  count(run, intervention, year) %>%
  group_by(year , intervention) %>% 
  summarise(n = mean(n)) %>%
  mutate(year = as.character(year),
         n = n*model_specification$population$scale_down_factor) %>%
  group_by(intervention) 


r %>%
  # simulation_results() %>% 
  filter(stroke != 0) %>%
  count(run, intervention, year) %>%
  group_by(intervention, year) %>%
  summarise(n = mean(n)) %>%
  mutate(year = as.character(year),
         n = n*model_specification$population$scale_down_factor) %>%
  group_by(intervention) %>% 
  e_charts(year) %>%
  e_line(n) %>%
  e_tooltip(trigger = "axis") %>%
  e_theme("walden") %>%
  e_y_axis(name = "Average Count") %>%
  e_grid(containLabel = TRUE) %>%
  e_x_axis(name = "Year")
