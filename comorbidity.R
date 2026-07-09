library(fst)
# source('app_prep.R')

#Risk Stratification

pop |> count(bmi)

pop$deprivation
pop$custom_townsend_rank
pop$custom_townsend_score_dz
pop$income_dm_decile_soa
pop$employment_dm_decile_soa
# Urban_mixed_rural_status
# NRA_name

bmi_age_10_bands_stacked <- pop |> count(age10,bmi) |>
  group_by(bmi) |>
      dplyr::filter(!is.na(bmi)) |>
      echarts4r::e_charts(age10, emphasis = list( focus = 'self')) |>
      echarts4r::e_bar(n, stack = "BMI") |>
      echarts4r::e_title("BMI distribution across Age Bands") |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_legend(show=T) |>
      echarts4r::e_x_axis(name = "Age Band") |>
      echarts4r::e_y_axis(name = "Population Count") |>
      # echarts4r::e_datazoom(xy = F) %>% 
  e_theme('walden')
  
metric_chart_bmi_age <- pop |> 
  dplyr::count(age10, bmi, name = "n") |>
  dplyr::filter(!is.na(bmi)) |>  
  group_by(bmi) |> 

  echarts4r::e_charts(age10, height = '100%', width = '100%',
                      emphasis = list( focus = 'self') ) |>
  echarts4r::e_bar(n) |>
   #echarts4r::e_title("BMI distribution across Age Bands") |>
  echarts4r::e_tooltip(trigger = "axis",confine=F) |>
  e_grid( containLabel = T )|> 
  e_theme('walden') %>% 
  echarts4r::e_legend(show=T, selector = TRUE) #%>% 
  # e_title('BMI with Age')
  
metric_chart_bmi_sex <- pop |> count(sex,bmi) |> 
  dplyr::filter(!is.na(bmi)) |>  
  group_by(bmi) |> 
  echarts4r::e_charts(sex,emphasis = list( focus = 'self')) |> #, height = '100%', width = '100%'
  e_grid( containLabel = T ,confine=T)|> 
  e_theme('walden') %>% 
  echarts4r::e_bar(n) |>
  #echarts4r::e_title("BMI distribution across Age Bands") |>
  echarts4r::e_tooltip(trigger = "axis",confine=T)

#############################################
# With the height and width both set to 100%
#############################################
(
metric_chart_bmi_sex <- pop |> count(sex,bmi) |> 
  dplyr::filter(!is.na(bmi)) |>  
  group_by(bmi) |> 
  echarts4r::e_charts(sex,emphasis = list( focus = 'self'), height = '100%', width = '100%') |> #, height = '100%', width = '100%'
  e_grid( containLabel = F ,confine=F)|> 
  e_theme('walden') %>% 
  echarts4r::e_bar(n) |>
  
  #echarts4r::e_title("BMI distribution across Age Bands") |>
  echarts4r::e_tooltip(trigger = "axis",confine=F)
)

fn_plt <- function(df,x_col,y_col=n, lab= ''){
  
  x_col      <- rlang::enquo(x_col)
  x_label <- rlang::quo_label(x_col)
  print(x_label)
  print(class(x_label))
  df <- df |> 
    rename(value = {{y_col}}) |>
    rename(x_col = {{x_col}}) 
  
if(is.logical(df$x_col)){
  print('logical')
  df <- df |> mutate(x_col =ifelse(x_col==T,
                             as.character(lab),
                             'normal')
    )
}
  
  print(df)
  df |> 
  dplyr::filter(!is.na(bmi)) |>  
  group_by(bmi) |> 
 # echarts4r::e_charts(x_col,height = '400px',width='300px') |>
    echarts4r::e_charts(x_col, height = '100%',width = '100%',emphasis = list( focus = 'self') ) %>%  # ,height = '100%',width = '100%'
    echarts4r::e_bar(value) |>
    e_grid( containLabel = T )|> 
    e_theme('walden') |>
  #echarts4r::e_title("BMI distribution across Age Bands") |>
  echarts4r::e_tooltip(trigger = "axis",confine=T)
}

(income_plot <- pop |> 
  count(income_dm_decile_soa,bmi) |> 
  fn_plt(x_col=income_dm_decile_soa) %>% 
    e_lm(x_col~bmi)
  )

employment_plot <- pop |> count(bmi,employment_dm_decile_soa)|> 
  fn_plt(x_col=employment_dm_decile_soa)

NRA_plot <- pop |> 
  # mutate(NRA_name = na_if(NRA_name, 'NA')) |> 
  count(bmi, NRA_name = 'NA'==NRA_name)|> 
  fn_plt(x_col = NRA_name, lab='NRA')

HSCT_plot <- pop |> 
  # mutate(NRA_name = na_if(NRA_name, 'NA')) |> 
  count(bmi, HSCT)|> 
  fn_plt(x_col = HSCT)

hypertension_plot <- pop |> 
  count(bmi,hypertension = hypertension_status != 'normotensive_untreated')|> 
  fn_plt(x_col=hypertension, lab = 'Hypertension') #|> 
  # rename(value = n) |>
  # rename(x_col = hypertension) |>
  # mutate(x_col= ifelse(is.logical(x_col),
  #               ifelse(x_col==T,as.character(x_col),'normal'),
  #               x_col))) |>
  # dplyr::filter(!is.na(bmi)) |>  
  # group_by(bmi) |> 
  # echarts4r::e_charts(x_col) |>
  # echarts4r::e_bar(value) |>
  # #echarts4r::e_title("BMI distribution across Age Bands") |>
  # echarts4r::e_tooltip(trigger = "axis")

af_plot <- pop |> count(bmi,atrial_fibrillation = (af_status == 'af')) |> 
  fn_plt(x_col=atrial_fibrillation, lab = 'Atrial Fibrillation')
ethnicity_plot <- pop |> count(bmi,ethnicity = broad_ethnicity)|> 
  fn_plt(x_col=ethnicity)
pad_plot <- pop |> count(bmi,pad = (pad_status == 'pad'))|> 
  fn_plt(x_col=pad, lab = 'Peripheral Arterial Disease')
ckd_plot <- pop |> count(bmi,ckd = (ckd_status == 'ckd'))    |> 
  fn_plt(x_col=ckd, lab = 'Early stage Kidney Disease')

cholesterol_plot <- pop |> count(bmi,cholesterol = (cholesterol_status=='raised_cholesterol'))|> 
  fn_plt(x_col=cholesterol, lab = 'High Cholesterol')

smoke_plot <- pop |> count(bmi,smoking = !is.na(smoking))  |> 
  fn_plt(x_col=smoking, lab = 'Smokes')
alcohol_plot <- pop |> count(bmi,alcohol = !is.na(alcohol)) |> 
  fn_plt(x_col=alcohol, lab = 'Unsafe Alcohol Consumption')
diet_plot <- pop |> count(bmi,diet = !is.na(diet))     |> 
  fn_plt(x_col=diet, lab = 'Unhealthy Diet')
pa_plot <- pop |> count(bmi,pa = !is.na(pa))      |> 
  fn_plt(x_col=pa, lab = 'Insufficient Physical Activity')

# pop |> count(bmi,wellbeing = !is.na(wellbeing))
# pop |> count(bmi,pm25g = !is.na(pm25g)) 

# pop <- pop |> 
#   rowwise() |> 
#   mutate(
#   comorbidities = sum(
# !is.na(diabetes),
# !is.na(atrial_fibrillation),
# !is.na(pad),
# !is.na(ckd),
# !is.na(hypertension),
# !is.na(cholesterol),
# 
# !is.na(bmi),
# !is.na(smoking),
# !is.na(alcohol),
# !is.na(diet),
# !is.na(pa)#,
# #!is.na(wellbeing)#,
# #!is.na(pm25g)
# )
# )

comorbidities_plot <- pop |> 
  filter(!is.na(bmi)) |>
  # count(bmi,comorbidities) |> 
  # fn_plt(x_col = comorbidities)
  count(bmi,multimorbidity) |> 
  fn_plt(x_col = multimorbidity)


# pop |> 
#   # ggplot(aes(comorbidities, n, fill=bmi))+
#   ggplot(aes(multimorbidity, n, fill=bmi))+=
#   geom_col(position = 'dodge')

hist(pop$multimorbidity)
hist((pop$qrisk_score))

#Causes of obesity
# DIET

# Pollution

( pm25g_bmi_chart_2 <-pop |>   
    filter(!is.na(bmi)) |> 
    mutate(bmi=factor(bmi,
                      levels = c('normal','overweight','obese'))) |>
    group_by(bmi) |> 
    summarise(pm25g = mean(pm25g)) |> 
    e_charts(bmi) |> 
    e_bar(pm25g) |> 
    # e_color(color = c('lightblue')) |> 
    e_lm(pm25g~bmi,name = 'trend') %>% 
    # e_line(pm25g,
    #        color='mediumseagreen',
    #        itemStyle = list(opacity=1, size=10),
    #        lineStyle = list(width=5)) |> 
    e_y_axis(min = 4, max = 5)   %>% 
    e_grid(  containLabel = T) 
)

(pm25g_bmi_scatter_chart <- pop |>
    filter(!is.na(bmi)) |>
    group_by(sdz_code, bmi)%>%
    summarise(pm25g = mean(pm25g),n=n()) |>
    add_count(sdz_code, wt = n, name = 'nn') %>% 
    mutate(n = n/nn) |>
    group_by(bmi) %>%
    e_charts(pm25g, emphasis = list( focus = 'series')) |>
    e_scatter(n, selectedMode = 'series' ) |>
    # e_color(color = c('lightblue')) |>
    e_lm(n ~ pm25g,
         name = rev(c('obese','overweight','normal'))
         ) %>%
    # e_line(pm25g,
    #        color='mediumseagreen',
    #        itemStyle = list(opacity=1, size=10),
    #        lineStyle = list(width=5)) |>
    # e_y_axis(max=40)   %>%
    e_grid(  containLabel = T) %>% 
    e_zigzag(axis ='x',start=0,end=2)   %>%
    e_theme('azul') %>% 
    e_axis(axis = 'y', formatter = e_axis_formatter('percent'))
)
  #!is.na(pm25g)

pop |>
  filter(!is.na(bmi)) |>
  group_by(sdz_code, bmi)%>%
  summarise(pm25g = mean(pm25g),n=n()) |>
  add_count(sdz_code, wt = n, name = 'nn') %>% 
  # mutate(n = n/nn) |>
  group_by(bmi) %>%
  e_charts(pm25g, emphasis = list( focus = 'series')) |>
  e_density(n, selectedMode = T) %>% 

  # e_histogram(n, selectedMode = T) #|>
  # e_color(color = c('lightblue')) |>
  
  # e_line(pm25g,
  #        color='mediumseagreen',
  #        itemStyle = list(opacity=1, size=10),
  #        lineStyle = list(width=5)) |>
  # e_y_axis(max=40)   %>%
  e_grid(  containLabel = T) %>% 
  e_theme('azul') %>% 
  e_axis(axis = 'y', formatter = e_axis_formatter('percent'))

(pm25g_urban_chart <- pop |>   
  filter(!is.na(bmi)) |> 
    mutate(Urban_mixed_rural_status=factor(Urban_mixed_rural_status,
                                           levels=c('Urban','Mixed','Rural'))) |> 
  group_by(Urban_mixed_rural_status) |> 
  summarise(pm25g = mean(pm25g)) |> 
  e_charts(Urban_mixed_rural_status,emphasis = list( focus = 'self')) |> 
  e_bar(pm25g) |> 
  # e_color(color = c('lightblue')) |> 
    e_lm(pm25g~Urban_mixed_rural_status,name = 'trend') %>% 
  # e_line(pm25g,
  #        color='mediumseagreen',
  #        itemStyle = list(opacity=1, size=10),
  #        lineStyle = list(width=5)) |> 
  e_zigzag(axis ='y',start=0,end=3)   %>% 
    e_grid(  containLabel = T) 
  )



#Depression
(depression_obesity_chart <- pop |>   
  filter(!is.na(bmi)) |> 
  group_by(bmi) |> 
  summarise(depression_percentile = mean(depression_percentile),n()) |> 
    
  e_charts(bmi,  height = '100%',width = '100%') |> 
  e_bar(name = 'Propensity for poor mental health',depression_percentile) |>
    e_y_axis(name = 'Propensity' ) |>
  # e_color(color = c('lightblue')) |> 
    e_lm(depression_percentile ~ bmi , name = 'trend') %>% 
  # e_line(legend = F, depression_percentile,
  #        #color='mediumseagreen',
  #        itemStyle = list(opacity=0),
  #        lineStyle = list(width=1,cap = 'round')) |> 
  e_grid(  containLabel = T )|> 
    e_theme('walden') |> 
    e_zigzag(axis ='y',start=0.1,end=0.4)   %>% 
    echarts4r::e_tooltip(trigger = "axis",confine=T)
)

( pop |>   
    filter(!is.na(bmi)) |> 
    group_by(bmi) |> 
    summarise(depression_percentile = sum(depression_percentile)) |> 
    e_charts(bmi) |> 
    e_bar(name = 'Propensity for poor mental health',depression_percentile) |>
    e_y_axis(name = 'Propensity' ) |>
    # e_color(color = c('lightblue')) |> 
    e_lm(depression_percentile ~ bmi ) %>% 
    # e_line(legend = F, depression_percentile,
    #        #color='mediumseagreen',
    #        itemStyle = list(opacity=0),
    #        lineStyle = list(width=1,cap = 'round')) |> 
    e_grid(  containLabel = T )|> 
  e_theme('walden') #|> 
    # e_y_axis(min=0.4,max=0.6) 
    )

##################################
##################################

####### Include Sleep percentile #########

##################################
##################################

pop |>
  filter(!is.na(bmi)) |>
  group_by(bmi) |>
  summarise(sleep_percentile = mean(sleep_percentile)) |>
  e_charts(bmi) |>
  e_bar(sleep_percentile, ) |>
  # e_color(color = c('lightblue')) |>
    e_zigzag(axis = 'y',start = 0.1,end = 0.4) %>%
  # e_line(sleep_percentile,
  #        color='lightblue',
  #        symbol='none',
  #        itemStyle = list(opacity=1),
  #        lineStyle = list(width=8,lineEnd='round')
  #        ) |>
  e_theme('walden')
  # e_y_axis(min=0.4,max=0.6)


(
sleep_bmi_chart <- pop |>
  filter(!is.na(bmi)) |>
  count(bmi,sleep) |>
  add_count(bmi, wt = n, name = 'nn') %>% 
    filter(sleep == 'sleep_apnea') %>% 
    mutate(sleep= 'Sleep Apnea') %>%
  mutate(n=n/nn) %>% 
    group_by(bmi) %>%
  e_charts(sleep) |>
  e_bar(n, barGap = 0) |> #, name = 'Sleep Apnea'
  e_zigzag(axis = 'y',start = 0.1,end = 0.8) %>%
  e_theme('walden') %>% 
  e_tooltip(formatter = e_tooltip_item_formatter('percent',digits = 2)) %>% 
  e_axis(axis = 'y', label = 'Sleep', formatter = e_axis_formatter('percent')) %>% 
  e_axis_labels(x = '', y = 'Proportion of Population')
)

(
townsend_distribution_chart <- pop |> 
  filter(!is.na(bmi)) %>% 
  # group_by(bmi) |>
  e_chart() |>
  e_histogram(stack = 'bmi',

            serie = custom_townsend_score_dz#,       

) |> 
  e_grid( containLabel = T)  |> 
  e_theme('walden')
)


pop |> 
  filter(!is.na(bmi)) %>%
  # group_by(bmi) |>
  # arrange(rev(bmi)) %>% 
  e_chart() |>
  echarts4r::e_tooltip(trigger = "axis") %>% 
  e_histogram(#stack = 'bmi',
              custom_townsend_score_dz    
        
  ) |> 
  e_grid( containLabel = T)  |> 
  e_theme('walden')

 pop |> 
    filter(!is.na(bmi)) %>%
    count(bmi,mdm_quintile_soa_name,mdm_quintile_soa) %>% 
   arrange(mdm_quintile_soa) %>% 
    group_by(bmi) |>
    e_chart(mdm_quintile_soa_name) |>
    e_line(n) %>% 
   e_lm(mdm_quintile_soa_name~bmi)

 pop %>% 
   group_by(mdm_quintile_soa_name) %>% 
   summarise(min(mdm_rank),
             max(mdm_rank))
 
 pop %>% 
   group_by(mdm_quintile_soa_name,mdm_quintile_soa) %>% 
   arrange(mdm_quintile_soa) %>% 
   summarise(min = min(custom_townsend_score_dz),
             avg = mean(custom_townsend_score_dz),
             max = max(custom_townsend_score_dz)
             ) %>% 
   arrange(mdm_quintile_soa)

(
qrisk_distribution_chart <- pop |> 
    filter(qrisk_score>0.02) |> 
  # group_by(bmi) |>
  e_chart() |> 
  e_density(serie = qrisk_score,         
            itemStyle = list(opacity=0),
            lineStyle = list(width=2)#,color='white'
  ) |> 
  e_theme('walden') |> 
    e_grid( containLabel = T) %>% 
    echarts4r::e_tooltip(trigger = "axis",confine=T)
)

# pop |>   
#   filter(!is.na(bmi)) |> 
#   group_by(bmi) |> 
#   summarise(pm25g = median(pm25g)) |> 
#   e_charts(bmi) |> 
#   e_bar(pm25g) |> 
#   e_line(pm25g,color='lightgreen') |> 
#   e_y_axis(min=4,max=4.5) 

pop |> 
  filter(!is.na(bmi)) |> 
  group_by(bmi) |> 
  e_charts() |> 
  e_boxplot(depression_percentile) %>% 
  e_grid(  containLabel = T ) |> 
  e_tooltip(confine=T) %>% 
  e_theme('walden') 
  


depression_obesity_chart
pm25g_urban_chart
pm25g_bmi_chart_2

townsend_distribution_chart
qrisk_distribution_chart

comorbidities_plot

income_plot
employment_plot
NRA_plot
HSCT_plot

metric_chart_bmi_sex
metric_chart_bmi_age

ethnicity_plot

hypertension_plot
af_plot
pad_plot
ckd_plot
cholesterol_plot

smoke_plot
alcohol_plot
diet_plot
pa_plot

