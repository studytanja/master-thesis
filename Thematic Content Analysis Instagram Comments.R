# Libraries
library(tidyverse)
library(readxl)
library(writexl)
library(tidyquant)
data_tbl <- read_excel("Data/Thematic Analysis @SamaOnTheMove - R Studio.xlsx")


# Pie Chart Analysis (Positive / Negative / Neutral) ----------------------

pie_chart_data_tbl <- data_tbl %>% 
    select(Positive, Negative, `Neutral/Unclear`) %>% 
    mutate(
        Positive = case_when(
            Positive == "X" ~ "1",
            TRUE ~ Positive),
        Negative =  case_when(
            Negative == "X" ~ "1",
            TRUE ~ Negative),
        `Neutral/Unclear` = case_when(
            `Neutral/Unclear` == "X" ~ "1"
        )
    ) %>% 
    mutate(Positive = Positive %>% as.integer(),
           Negative = Negative %>% as.integer(),
           `Neutral/Unclear` = `Neutral/Unclear` %>% as.integer()) 


# Exploring the Distribution of Positive, Neutral and Negative ------------


pie_chart_data_tbl$Positive %>% sum(na.rm = TRUE )/ nrow(pie_chart_data_tbl)

pie_chart_data_tbl$Negative %>% sum(na.rm = TRUE )/ nrow(pie_chart_data_tbl)

pie_chart_data_tbl$`Neutral/Unclear` %>% sum(na.rm = TRUE )/ nrow(pie_chart_data_tbl)


# Data Wrangling ----------------------------------------------------------

# Changing the Data to Boolean

pie_chart_data_tbl <- pie_chart_data_tbl %>% 
  mutate(Positive = !is.na(Positive),
         Negative = !is.na(Negative),
         `Neutral/Unclear` = !is.na(`Neutral/Unclear`))

# Pivoting Long

pie_chart_data_tbl <-  pie_chart_data_tbl %>% 
  pivot_longer(cols = 1:3, names_to = "Sentiment", values_to = "Value")

# Group By and Summarize

pie_chart_data_tbl <- pie_chart_data_tbl %>% 
  group_by(Sentiment) %>% 
  summarise(Total = sum(Value))

# Add percentages

pie_chart_data_tbl <- pie_chart_data_tbl %>% 
  mutate(Percentage = scales::percent(Total/sum(Total)))

# Pie Chart Visualization -------------------------------------------------

pie_chart_data_tbl %>%
  ggplot(aes(x = "", y = Total, fill = Sentiment)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Comments' Sentiment",
       y = "",
       x = "") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic")) +
  geom_text(aes(label = Percentage),
            position = position_stack(vjust = 0.5),
            # size = 4.5,
            hjust = "center", 
            fontface = "bold") +
  scale_fill_manual(values = c("Positive" = "#67B64D",
                               "Neutral/Unclear" = "#7C9CF7",
                               "Negative" = "#ff5a5e"))


#  Bar Chart Analysis (Frequency of Codes) ----------------------

# Data Wrangling ----------------------------------------------------------

bar_chart_data_tbl <- data_tbl %>% select(`Code 1`, `Code 2`, `Code 3`)

# Pivoting Long

bar_chart_data_tbl_long <- bar_chart_data_tbl %>%
    pivot_longer(cols = starts_with("code"),
                 names_to = "code_number",
                 values_to = "code") %>%
    filter(!is.na(code))

# Count Frequency

bar_chart_data_tbl_long_code_counts <- bar_chart_data_tbl_long %>%
    count(code, sort = TRUE)

# Select Top 10 Codes

bar_chart_data_tbl_top15 <- bar_chart_data_tbl_long_code_counts %>%
    slice_max(n, n = 15)


#  Bar Chart Visualization Top 15 --------------------------

bar_chart_data_tbl_top15 %>%
    ggplot(aes(x = reorder(code, n), y = n)) +
    geom_col(fill = "#4C72B0", width = 0.7) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5, color = "gray20") +
    coord_flip() +
    labs(
        title = "Frequency of Codes in Instagram Comments",
        x = NULL,
        y = "Frequency"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "gray30"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "gray30", face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 9, color = "gray40", hjust = 0)
    ) +
    expand_limits(y = max(bar_chart_data_tbl_long_code_counts$n) * 1.1)


#  Stacked Bar Chart Analysis (Sentiment per Codes) ----------------------

# Data Wrangling ----------------------------------------------------------

# Pivot Codes to Long Format

stacked_bar_chart_long <- data_tbl %>%
    pivot_longer(cols = starts_with("code"), names_to = "code_number", values_to = "code") %>%
    filter(!is.na(code))

# Add Sentiment

stacked_bar_chart_long <- stacked_bar_chart_long %>%
    mutate(sentiment = case_when(
        Positive == "X" ~ "Positive",
        Negative == "X" ~ "Negative",
        `Neutral/Unclear`== "X" ~ "Neutral/Unclear",
        TRUE ~ "unknown"
    ))

# Count Combinations of Code and Sentiment

stacked_bar_chart_long_code_sentiment_counts <- stacked_bar_chart_long %>%
    count(code, sentiment)

# Determine Top 15 Codes by Total Frequency ------------------------------

top15_codes <- stacked_bar_chart_long_code_sentiment_counts %>%
    group_by(code) %>%
    summarise(total_n = sum(n)) %>%
    slice_max(total_n, n = 15) %>%
    pull(code)

# Filter to Top 15 Codes Only --------------------------------------------

stacked_bar_chart_long_top15 <- stacked_bar_chart_long_code_sentiment_counts %>%
    filter(code %in% top15_codes)


# Stacked Bar Chart Visualization ------------------------------------

stacked_bar_chart_long_top15 %>%
    ggplot(aes(x = reorder(code, n), y = n, fill = sentiment)) +
    geom_bar(stat = "identity", width = 0.7, color = "white") +
    coord_flip() +
    labs(
        title = "Code Frequencies by Sentiment",
        x = "",
        y = "Frequency",
        fill = "Sentiment"
    ) +
    scale_fill_manual(values = c(
        "Positive" = "#67B64D",
        "Neutral/Unclear" = "#7C9CF7",
        "Negative" = "#FF5A5E"
    )) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "gray30", face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(color = "gray20"),
        plot.title = element_text(face = "bold", size = 16)
    )


