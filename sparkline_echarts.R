e_charts(data = data.frame(x=1:10,y=1:10),x) %>% 
  # e_line(y) %>% 
  # df |>
  # e_charts(x) |>
  e_line(y, smooth = TRUE,    lineStyle = list(opacity = 1),
  itemStyle = list(opacity = 0.0)) |>
  
  # hide the line + symbols
  # e_line(
  #   lineStyle = list(opacity = 0),
  #   itemStyle = list(opacity = 0),
  #   symbol = "none"
  # ) |>
  
  # keep tooltip
  e_tooltip(trigger = "axis") |>
  
  # remove axes
  e_x_axis(show = FALSE) |>
  e_y_axis(show = FALSE) |>
  
  # remove grid padding
  e_grid(
    left = 0, right = 0, top = 0, bottom = 0,
    containLabel = FALSE
  ) |>
  
  # remove legend
  e_legend(show = FALSE)
