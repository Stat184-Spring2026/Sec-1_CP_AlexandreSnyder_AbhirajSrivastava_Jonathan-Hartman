#this is more packages than I need but im just putting everything here
#install.packages("ggradar")
#install.packages("devtools")
#devtools::install_github("ricardo-bion/ggradar")
#install.packages("ggplot2")
#install.packages("tidyr)
#install.packages("fsmb")
#install.packages("scales")
#install.packages("knitr")
#install.packages("kableExtra")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggradar)
library(fmsb)
library(scales)
library(knitr)
library(kableExtra)


#Wrangling, this section is adapted from Gradient-table-code.R (variable names are shortened)
exclude_vals <- c(-100, -99, -98, -97, -96, -95, -94, -93, -90, -80, -70, -60, -40)

summary_table <- GSS_clean |>
  filter(YEAR %in% c(2018, 2021, 2022, 2024)) |>
  mutate(across(c(PRAY, HAPPY, CONCLERG, CONFED, CONMEDIC),
                ~ ifelse(. %in% exclude_vals, NA, .))) |>
  group_by(YEAR) |>
  summarise(
    Prayer = mean(PRAY, na.rm = TRUE) / 2,
    Happiness = mean(HAPPY, na.rm = TRUE),
    `Clergy Conf.` = mean(CONCLERG, na.rm = TRUE),    
    `Govt. Conf.` = mean(CONFED, na.rm = TRUE),     
    `Health Conf.` = mean(CONMEDIC, na.rm = TRUE) 
  ) |>
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "mean") |>
  pivot_wider(names_from = YEAR, values_from = mean)

#Transform dataframe so years are the rows
transformed_summary_table <- summary_table |>
  pivot_longer(cols = -variable, names_to = "year", values_to = "value") |>
  pivot_wider(id_cols = year, names_from = variable, values_from = value)

#Single Comparison Chart
radar_data <- transformed_summary_table
rownames(radar_data) <- radar_data$year
radar_data <- radar_data[, -1] #take year column out

#set scale and margins
radar_data <- rbind(
  rep(2.40, ncol(radar_data)),
  rep(1.40, ncol(radar_data)),
  radar_data
)
par(mar = c(1, 1, 2, 1))

#comparison chart
radarchart(radar_data,
           axistype = 1,
           pcol = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4"),
           pfcol = c(rgb(255,107,107, alpha=100, maxColorValue=255),
                     rgb(78,205,196, alpha=100, maxColorValue=255),
                     rgb(69,183,209, alpha=100, maxColorValue=255),
                     rgb(150,206,180, alpha=100, maxColorValue=255)),
           plwd = 3,
           cglcol = "grey",
           cglty = 1,
           cglwd = 0.8,
           axislabcol = "black",
           title = "Survey Responses by Year",
           caxislabels = c("1.40", "1.65", "1.90", "2.15", "2.40"),
           vlcex = .6,        # Variable labels (Clergy Conf., etc.) - 1.2x larger
           cex.main = 1,     # Title size - 1.8x larger
           calcex = .4,       # Axis numbers (1.40, 1.65, etc.) - 0.9x
           
        )

# add legend (made smaller to not conflict with labels)
legend(x = 1.7, y = 1.15,
       legend = transformed_summary_table$year,
       fill = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4"),
       title = "Year",
       cex = 0.5,
       pt.cex = 0.5,
       title.cex = 0.5,
       bty = "n",
       xpd = TRUE)
par(mar = c(.5, .4, .4, .2) + 0.1)


#four seperate charts

#set a grid
par(mfrow = c(2, 2))
par(mar = c(1, 1, 3, 1))

#set colors to years
year_colors <- list(
  "2018" = "#FF6B6B",
  "2021" = "#4ECDC4",
  "2022" = "#45B7D1",
  "2024" = "#96CEB4"
)

#Create seperate radar charts with loops
for(year_name in transformed_summary_table$year) {
  year_data <- transformed_summary_table |>
    filter(year == year_name) |>
    select(-year)
  
  year_matrix <- rbind(
    rep(2.40, ncol(year_data)),
    rep(1.40, ncol(year_data)),
    year_data
  )

  #radar charts
  radarchart(year_matrix,
             axistype = 1,
             pcol = year_colors[[year_name]],
             pfcol = adjustcolor(year_colors[[year_name]], alpha.f = 0.4),
             plwd = 3,
             cglcol = "grey",
             cglty = 1,
             cglwd = 0.8,
             axislabcol = "black",
             vlcex = 1.1,
             title = paste("Year:", year_name),
             caxislabels = c("1.40", "1.65", "1.90", "2.15", "2.40"))
}

#fix layout
par(mfrow = c(1, 1))
par(mar = c(.5, .4, .4, .2) + 0.1)