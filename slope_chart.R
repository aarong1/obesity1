library(dplyr)
library(echarts4r)
library(htmltools)

slope_like_chart <- function(df, highlight = c("Germany","France","Italy","Spain","UK")) {
  df <- df %>%
    mutate(
      x = as.character(x),
      highlight = country %in% highlight
    )
  
  # first/last x values (for endpoints)
  x_levels <- df %>% distinct(x) %>% arrange(x) %>% pull(x)
  x_left  <- x_levels[1]
  x_right <- x_levels[length(x_levels)]
  
  left_pts <- df %>%
    filter(x == x_left) %>%
    mutate(
      label = paste0(country, " ", formatC(value, format = "f", digits = 1))
    )
  
  right_pts <- df %>%
    filter(x == x_right) %>%
    mutate(
      label = paste0(formatC(value, format = "f", digits = 1), " ", country)
    )
  
  # split lines
  df_grey  <- df %>% filter(!highlight)
  df_black <- df %>% filter(highlight)
  
  e_charts() %>%
    # --- grey background lines ---
    e_line(
      data = df_grey,
      x = x, y = value, serie = country,
      showSymbol = FALSE,
      legendHoverLink = FALSE
    ) %>%
    e_line_style(width = 2, opacity = 0.30) %>%
    e_item_style(color = "#BDBDBD") %>%
    
    # --- highlighted lines ---
    e_line(
      data = df_black,
      x = x, y = value, serie = country,
      showSymbol = FALSE
    ) %>%
    e_line_style(width = 4, opacity = 1) %>%
    e_item_style(color = "#111111") %>%
    
    # --- endpoint dots + labels (LEFT) ---
    e_scatter(
      data = left_pts,
      x = x, y = value, serie = "left",
      symbolSize = 12
    ) %>%
    e_item_style(
      color = ~ ifelse(highlight, "#111111", "#D0D0D0")
    ) %>%
    e_labels(
      show = TRUE,
      position = "left",
      formatter = htmlwidgets::JS("function(p){ return p.data.label; }")
    ) %>%
    
    # --- endpoint dots + labels (RIGHT) ---
    e_scatter(
      data = right_pts,
      x = x, y = value, serie = "right",
      symbolSize = 12
    ) %>%
    e_item_style(
      color = ~ ifelse(highlight, "#111111", "#D0D0D0")
    ) %>%
    e_labels(
      show = TRUE,
      position = "right",
      formatter = htmlwidgets::JS("function(p){ return p.data.label; }")
    ) %>%
    
    # --- styling to match the screenshot vibe ---
    e_legend(show = FALSE) %>%
    e_tooltip(trigger = "item") %>%
    e_grid(left = 40, right = 40, top = 20, bottom = 20, containLabel = TRUE) %>%
    e_x_axis(
      type = "category",
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = list(show = FALSE),
      splitLine = list(show = TRUE)  # faint verticals
    ) %>%
    e_y_axis(
      type = "value",
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = list(show = FALSE),
      splitLine = list(show = FALSE)
    )
}

# ---- example usage ----
# df must be long: country, x, value
# df <- your_data
# slope_like_chart(df, highlight = c("Germany","France","Italy","Spain","UK"))