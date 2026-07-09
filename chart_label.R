library(echarts4r)
library(htmlwidgets)

Icons <- list(
  Car = "./www/car_van_2_icon_png.png",
  Overcrowding = "www/25694.png",
  Job = "./briefcase_2_icon_png.png",
  Ownership = "key.png"
)

df <- data.frame(
  category = c("Car", "Overcrowding", "Job", "Ownership"),
  value = c(80, 60, 40, 80)
)

x <- df |>
  e_charts(category) |>
  e_bar(
    value,
    name = "DZ",
    itemStyle = list(
      borderRadius = 5,
      color = "#000000"
    )
  ) |>
  e_flip_coords() |>
  e_grid(left = 100) |>
  e_tooltip(
    trigger = "axis",
    axisPointer = list(type = "shadow")
  ) |>
  e_toolbox_feature("saveAsImage") |>
  e_x_axis(
    type = "value",
    splitLine = list(show = FALSE),
    axisLine = list(show = FALSE),
    axisTick = list(show = FALSE),
    axisLabel = list(show = FALSE)
  ) |>
  e_y_axis(
    type = "category",
    inverse = FALSE,
    data = df$category,
    splitLine = list(show = FALSE),
    axisLine = list(show = FALSE),
    axisTick = list(show = FALSE),
    axisLabel = list(
      align = "right",
      textStyle = list(align = "right"),
      formatter = JS("
        function(value) {
          return '{' + value + '| }\\n{value|' + value + '}';
        }
      "),
      margin = 20,
      rich = list(
        value = list(
          lineHeight = 20,
          align = "right",
          color = "#000000",
          fontWeight = "bold"
        ),
        Car = list(
          height = 20,
          align = "right",
          backgroundColor = list(image = Icons$Car),
          borderRadius = 10,
          borderWidth = 0,
          borderColor = "#ff8811"
        ),
        Overcrowding = list(
          height = 20,
          align = "right",
          backgroundColor = list(image = Icons$Overcrowding),
          borderRadius = 10,
          borderWidth = 0,
          borderColor = "#ff8811"
        ),
        Job = list(
          height = 20,
          align = "right",
          backgroundColor = list(image = Icons$Job),
          borderRadius = 10,
          borderWidth = 0,
          borderColor = "#ff8811"
        ),
        Ownership = list(
          height = 20,
          align = "right",
          backgroundColor = list(image = Icons$Ownership),
          borderRadius = 10,
          borderWidth = 0,
          borderColor = "#ff8811"
        )
      )
    )
  )

ui <- div( 
  
  x
)

server <- function(input, output, session) {
  output$chart <- renderEcharts4r({
    x
  })
}

shinyApp(ui, server)
