



library(rbokeh)
library(readxl)
tendulkar_100s_data_v2 <- read_excel("~/Desktop/tendulkar_100s_data.xlsx")

suppressWarnings(figure(title = "Sachin Tendulkar's International Centuries Across the Globe", width = 800, height = 450, padding_factor = 0) %>%
ly_map("world", col = "gray") %>%
ly_points(Longitude, Latitude, data = tendulkar_100s_data_v2, size = 5,
hover = c(century_number, Runs, Opposition, Stadium, Date))) %>%
x_axis(visible = FALSE, grid = FALSE) %>%
y_axis(visible = FALSE, grid = FALSE)