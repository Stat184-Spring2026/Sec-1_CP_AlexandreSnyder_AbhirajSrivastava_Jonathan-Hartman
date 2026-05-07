#install.packages("dplyr")
#install.packages("knitr")
#install.packages("tidyr")
#install.packages("kableExtra")

library(dplyr)
library(knitr)
library(tidyr)
library(kableExtra)

# make a vector with excluded values (defined in the social survey codebook)
exclude_vals <- c(-100, -99, -98, -97, -96, -95, -94, -93, -90, -80, -70, -60, -40)

#get averages
summary_table <- GSS_clean |>
  
#filter for specified years
  filter(YEAR %in% c(2018, 2021, 2022, 2024)) |>
  mutate(across(c(PRAY, HAPPY, CONCLERG, CONFED, CONMEDIC),
                ~ ifelse(. %in% exclude_vals, NA, .))) |>
#calculate means (prayer divided by two to match the 1-3 scores)
   group_by(YEAR) |>
  summarise(
    Prayer = mean(PRAY, na.rm = TRUE) / 2,
    Happiness = mean(HAPPY, na.rm = TRUE),
    `Clergy Confidence` = mean(CONCLERG, na.rm = TRUE),
    `Govt Confidence` = mean(CONFED, na.rm = TRUE),
    `Health Confidence` = mean(CONMEDIC, na.rm = TRUE)
  ) |>
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "mean") |>
  pivot_wider(names_from = YEAR, values_from = mean)

#Create a separate dataframe with the formatted cells
formatted_table <- summary_table

#make objects with gradient values
green_anchor <- 1.43
white_anchor <- 2.38

#loop through each cell in the table and set the color value relative to the gradient values 
for(i in 1:nrow(summary_table)) {
  for(col in c("2018", "2021", "2022", "2024")) {
    
    val <- summary_table[[col]][i]
    
    if(!is.na(val) && is.numeric(val)) {
      if(val <= green_anchor) {
        intensity <- 1 #white
      } else if(val >= white_anchor) {
        intensity <- 0 #green
      } else {
        intensity <- 1 - ((val - green_anchor) / (white_anchor - green_anchor))
      }
      intensity <- max(0, min(1, intensity))
#set intensity values to actual RGB codes
      red_blue_value <- round(255 - (intensity * 255))
      green_value <- 255
      bg_color <- rgb(red_blue_value, green_value, red_blue_value, maxColorValue = 255)

#text readability
      formatted_table[[col]][i] <- cell_spec(round(val, 2), 
                                             background = bg_color,
                                             color = ifelse(intensity > 0.6, "white", "black"))
    } else {
      formatted_table[[col]][i] <- cell_spec("NA", background = "#f0f0f0", color = "gray")
    }
  }
}

#final kable table
styled_table <- formatted_table |>
  kable(format = "html", 
        caption = "<b style='color:black;'>Table 1: Mean Responses by Variable and Year</b>", 
        
styled_table
        escape = FALSE, 
        align = "c") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, 
                font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#f0f0f0") |>
  footnote(general = paste(
    "Note: Variable scales - PRAY: 1 (Several times/day) to 6 (Never), divided by 2 for 0.5-3 scale;",
    "HAPPY: 1 (Very happy) to 3 (Not too happy);",
    "CONCLERG: 1 (A great deal of confidence in clergy) to 3 (Hardly any);",
    "CONFED: 1 (A great deal of confidence in federal government) to 3 (Hardly any);",
    "CONMEDIC: 1 (A great deal of confidence in medicine) to 3 (Hardly any).",
    "Green gradient: Darker green = closer to 1 (more positive response), White = closer to 3 (more negative response)."
  ))