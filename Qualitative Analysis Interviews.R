# Libraries
library(tidyverse)
library(readxl)
library(writexl)
library(tidyquant)
data_tbl <- read_excel("Data/Qualitative Content Analysis Interviews - R Studio.xlsx")

#  Bar Chart Analysis (Frequency of Codes) ----------------------

# Data Wrangling ----------------------------------------------------------

bar_chart_data_tbl <- data_tbl %>% select(`Code 1`, `Code 2`, `Code 3`, `Code 4`, `Code 5`)

# Pivoting Long

bar_chart_data_tbl_long <- bar_chart_data_tbl %>%
    pivot_longer(cols = starts_with("code"),
                 names_to = "code_number",
                 values_to = "code") %>%
    filter(!is.na(code))

# Count Frequency

bar_chart_data_tbl_long_code_counts <- bar_chart_data_tbl_long %>%
    count(code, sort = TRUE)

# Select Top 17 Codes

bar_chart_data_tbl_top17 <- bar_chart_data_tbl_long_code_counts %>%
    slice_max(n, n = 17)

# Reorder factor levels (so bars appear in descending order)
bar_chart_data_tbl_top17 <- bar_chart_data_tbl_top17 %>%
    mutate(code = fct_reorder(code, n, .desc = TRUE))


# Bar Chart Visualization -----------------------------------------

bar_chart_data_tbl_top17 %>%
    ggplot(aes(x = reorder(code, n), y = n)) +
    geom_col(fill = "#4C72B0", width = 0.7) +
    geom_text(
        aes(label = n),
        hjust = -0.2,
        size = 3.5,
        color = "gray20"
    ) +
    coord_flip() +
    labs(
        title = "Frequency of Codes in Interviews",
        x = "",
        y = "Frequency"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "gray30", face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 20, 10, 10)
    ) +
    expand_limits(y = max(bar_chart_data_tbl_long_code_counts$n) * 1.1)

