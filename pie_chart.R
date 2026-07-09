
pop %>% 
  count(bmi,name='count') %>% 
  mutate(count = count*model_specification$population$scale_down_factor) %>% 
  filter_out(is.na(bmi)) %>% 
  e_charts(bmi, emphasis = list(
    focus = 'self')) |> 
  e_pie(count) |> 
  e_title('BMI Breakdown') |> 
  e_tooltip() %>% 
  e_theme(name = 'walden') 
      




