# Guidelines for Analyzing Classic Novels in R with Improved Plot Visibility
# This script replicates the analysis of "The Adventures of Huckleberry Finn" and "Little Women"
# and enhances the scatter plot for better readability by adjusting axis scales, point sizes, and more.

# Step 1: Install and load necessary packages
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stringr)) install.packages("stringr")
if (!require(tibble)) install.packages("tibble")

library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)

# Step 2: Download text from Project Gutenberg
huck_finn_url <- "http://www.gutenberg.org/files/76/76-0.txt"
little_women_url <- "http://www.gutenberg.org/files/37106/37106-0.txt"

download_text <- function(url) {
  tryCatch({
    con <- url(url, open = "r", encoding = "UTF-8")
    on.exit(close(con))
    text <- paste(readLines(con, warn = FALSE), collapse = "\n")
    Encoding(text) <- "UTF-8"
    return(text)
  }, error = function(e) {
    stop("Failed to download text from ", url, ": ", e$message)
  })
}

huck_finn_text <- download_text(huck_finn_url)
little_women_text <- download_text(little_women_url)

# Step 3: Split text into chapters
huck_finn_chapters <- str_split(huck_finn_text, "CHAPTER ")[[1]][44:length(str_split(huck_finn_text, "CHAPTER ")[[1]])]
little_women_chapters <- str_split(little_women_text, "CHAPTER [A-Z]+\\n")[[1]][2:length(str_split(little_women_text, "CHAPTER [A-Z]+\\n")[[1]])]

# Step 4: Create a table of chapters
huck_finn_table <- tibble(Chapters = huck_finn_chapters)
little_women_table <- tibble(Chapters = little_women_chapters)

# Display first 10 chapters of Huckleberry Finn
print(huck_finn_table[1:10, ])

# Step 5: Analyze periods and characters
analyze_chapter <- function(chapter) {
  periods <- str_count(chapter, "\\.")
  characters <- nchar(chapter)
  return(tibble(Periods = periods, Characters = characters))
}

huck_finn_stats <- huck_finn_table %>%
  rowwise() %>%
  do(analyze_chapter(.$Chapters)) %>%
  mutate(Book = "Huckleberry Finn")

little_women_stats <- little_women_table %>%
  rowwise() %>%
  do(analyze_chapter(.$Chapters)) %>%
  mutate(Book = "Little Women")

combined_stats <- bind_rows(huck_finn_stats, little_women_stats)

# Step 6: Visualize with improved readability
# Create a scatter plot with log scales, larger points, transparency, and formatted axes
p <- ggplot(combined_stats, aes(x = Periods, y = Characters, color = Book)) +
  geom_point(size = 3, alpha = 0.6) + # Larger points with transparency
  scale_x_log10(labels = scales::comma, breaks = c(10, 50, 100, 200, 500)) + # Log scale for X, readable breaks
  scale_y_log10(labels = scales::comma, breaks = c(1000, 5000, 10000, 20000, 50000)) + # Log scale for Y
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + # Add regression line
  labs(title = "Periods vs. Characters in Chapters",
       subtitle = "Huckleberry Finn and Little Women (Log Scales)",
       x = "Number of Periods (Log Scale)",
       y = "Number of Characters (Log Scale)") +
  theme_minimal(base_size = 14) + # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Display the plot
print(p)

# Save the plot with larger dimensions for clarity
ggsave("chapter_stats_plot.png", plot = p, width = 10, height = 6, dpi = 300)

# Step 7: Alternative plot with linear scales and zoomed range (optional)
p_linear <- ggplot(combined_stats, aes(x = Periods, y = Characters, color = Book)) +
  geom_point(size = 3, alpha = 0.6) +
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 25000)) + # Zoom in on dense area
  scale_x_continuous(breaks = seq(0, 500, 100), labels = scales::comma) +
  scale_y_continuous(breaks = seq(0, 25000, 5000), labels = scales::comma) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Periods vs. Characters in Chapters (Zoomed)",
       subtitle = "Huckleberry Finn and Little Women",
       x = "Number of Periods",
       y = "Number of Characters") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Display the alternative plot
print(p_linear)

# Save the alternative plot
ggsave("chapter_stats_plot_linear.png", plot = p_linear, width = 10, height = 6, dpi = 300)

# Notes:
# - Log scales handle large ranges but may distort perception; try the linear plot for a zoomed view.
# - Adjust size, alpha, or coord_cartesian() limits based on data distribution.
# - Inspect combined_stats to determine appropriate axis ranges (e.g., summary(combined_stats$Periods)).
# - Use ggsave() to export high-resolution plots for detailed inspection.