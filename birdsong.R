# ---- Install Required Packages ----
required_packages <- c(
  "tidyverse", "patchwork", "scales", "gridExtra",
  "RColorBrewer", "ggnewscale", "devtools"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) {
  install.packages(to_install)
}

if (!"ggradar" %in% installed) {
  devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
}

# ---- Load Required Packages ----
library(tidyverse)
library(lubridate)
library(scales)
library(RColorBrewer)
library(patchwork)
library(gridExtra)
library(ggnewscale)
library(ggradar)

# ---- Load and Prepare Data ----
bird_data <- read.csv("Birdsong.csv")

bird_data <- bird_data %>%
  mutate(
    Timestamp = mdy_hms(paste(Date, Observation_Start_PST)),
    Hour = hour(Timestamp) + minute(Timestamp) / 60 + second(Timestamp) / 3600,
    End_Hour = Hour + Observation_Duration / 3600,
    Date_Parsed = mdy(Date),
    Date_Label = format(mdy(Date), "%m/%d/%Y"),
    Obs_Label = paste0(SiteID, " (", Date_Label, ")")
  )

species_codes <- c("SS", "ST", "LG", "BCC", "BW", "GCS", "RCK", "Non_SS", "All")
for (sp in species_codes) {
  singing_col <- paste0("Singing_", sp)
  calling_col <- paste0("Chips_Calls_", sp)
  status_col <- paste0("Birdsong_Status_", sp)
  bird_data[[status_col]] <- case_when(
    bird_data[[singing_col]] == 1 ~ "Singing",
    bird_data[[calling_col]] == 1 ~ "Calling",
    bird_data[[singing_col]] == 0 & bird_data[[calling_col]] == 0 ~ "Silent",
    TRUE ~ "Unknown"
  )
  bird_data[[status_col]] <- factor(bird_data[[status_col]], levels = c("Silent", "Singing", "Calling", "Unknown"))
}

# ---- Marginal Histogram of Dog Count vs Noise Level ----
heat_df <- bird_data %>%
  filter(!is.na(Total_Dogs), !is.na(Dog_Noise_Max)) %>%
  mutate(
    Total_Dogs = as.factor(Total_Dogs),
    Dog_Noise_Max = as.factor(Dog_Noise_Max)
  ) %>%
  count(Dog_Noise_Max, Total_Dogs) %>%
  complete(Dog_Noise_Max, Total_Dogs, fill = list(n = 0))

max_top <- heat_df %>% group_by(Total_Dogs) %>% summarise(n = sum(n)) %>% pull(n) %>% max()
max_right <- heat_df %>% group_by(Dog_Noise_Max) %>% summarise(n = sum(n)) %>% pull(n) %>% max()

heat_plot <- ggplot(heat_df, aes(x = Total_Dogs, y = Dog_Noise_Max, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, color = n > 100), size = 4) +
  scale_fill_gradient(low = "#FFE5E0", high = "#F8766D") +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  labs(x = "Number of Dogs", y = "Dog Noise Level", fill = "Count") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

top_hist <- heat_df %>%
  group_by(Total_Dogs) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(x = Total_Dogs, y = n)) +
  geom_col(fill = "#F8766D") +
  geom_text(aes(label = n), vjust = -0.3, size = 4, color = "black") +
  theme_void() +
  ylim(0, max_top * 1.15)

right_hist <- heat_df %>%
  group_by(Dog_Noise_Max) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(x = n, y = Dog_Noise_Max)) +
  geom_col(fill = "#F8766D") +
  geom_text(aes(label = n), hjust = -0.2, size = 4, color = "black") +
  theme_void() +
  xlim(0, max_right * 1.15)

final_plot <- (top_hist + plot_spacer()) /
  (heat_plot + right_hist) +
  plot_layout(heights = c(1, 4), widths = c(4, 1)) +
  plot_annotation(
    title = "Marginal Histogram of Dog Count vs Noise Level",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(final_plot)

# ---- Marginal Histogram of Human Count vs Noise Level ----
human_df <- bird_data %>%
  filter(!is.na(Total_Humans), !is.na(Human_Noise_Max)) %>%
  mutate(
    Total_Humans = as.factor(Total_Humans),
    Human_Noise_Max = as.factor(Human_Noise_Max)
  ) %>%
  count(Human_Noise_Max, Total_Humans) %>%
  complete(Human_Noise_Max, Total_Humans, fill = list(n = 0))

max_top <- human_df %>% group_by(Total_Humans) %>% summarise(n = sum(n)) %>% pull(n) %>% max()
max_right <- human_df %>% group_by(Human_Noise_Max) %>% summarise(n = sum(n)) %>% pull(n) %>% max()

heat_plot <- ggplot(human_df, aes(x = Total_Humans, y = Human_Noise_Max, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, color = n > 100), size = 4) +
  scale_fill_gradient(low = "#E6FFE9", high = "#00BA38") +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  labs(x = "Number of Humans", y = "Human Noise Level", fill = "Count") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

top_hist <- human_df %>%
  group_by(Total_Humans) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(x = Total_Humans, y = n)) +
  geom_col(fill = "#00BA38") +
  geom_text(aes(label = n), vjust = -0.3, size = 4, color = "black") +
  theme_void() +
  ylim(0, max_top * 1.15)

right_hist <- human_df %>%
  group_by(Human_Noise_Max) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(x = n, y = Human_Noise_Max)) +
  geom_col(fill = "#00BA38") +
  geom_text(aes(label = n), hjust = -0.2, size = 4, color = "black") +
  theme_void() +
  xlim(0, max_right * 1.15)

final_plot <- (top_hist + plot_spacer()) /
  (heat_plot + right_hist) +
  plot_layout(heights = c(1, 4), widths = c(4, 1)) +
  plot_annotation(
    title = "Marginal Histogram of Human Count vs Noise Level",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(final_plot)

# ---- Histogram of Observation Duration by User Group ----
bird_data <- bird_data %>%
  filter(!is.na(User_Group_Composition)) %>%
  mutate(
    User_Group_Composition = recode(User_Group_Composition,
                                    "No users" = "No users",
                                    "No dogs" = "No dogs",
                                    "Dogs" = "Dogs"
    ),
    User_Group_Composition = factor(User_Group_Composition, levels = c("No users", "No dogs", "Dogs"))
  )

avg_med_df <- bird_data %>%
  group_by(User_Group_Composition) %>%
  summarise(
    avg = mean(Observation_Duration, na.rm = TRUE),
    med = median(Observation_Duration, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(bird_data, aes(x = Observation_Duration)) +
  geom_histogram(binwidth = 10, boundary = 5, fill = "#619CFF", color = "white", alpha = 0.8) +
  stat_bin(binwidth = 10, boundary = 5, geom = "text",
           aes(label = after_stat(count)),
           vjust = -0.5, size = 3.2, color = "black") +
  geom_density(aes(y = ..count.. * 10), color = "darkblue", size = 1.2, adjust = 1.1) +
  geom_vline(data = avg_med_df, aes(xintercept = avg, color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(data = avg_med_df, aes(xintercept = med, color = "Median"), linetype = "dashed", linewidth = 1) +
  geom_text(data = avg_med_df, aes(x = avg, y = 85, label = paste0("Mean: ", round(avg, 1), "s")),
            color = "purple", hjust = -0.1, size = 3.5, inherit.aes = FALSE) +
  geom_text(data = avg_med_df, aes(x = med, y = 75, label = paste0("Med: ", round(med, 1), "s")),
            color = "orange", hjust = -0.1, size = 3.5, inherit.aes = FALSE) +
  scale_color_manual(name = "Reference Line", values = c("Mean" = "purple", "Median" = "orange")) +
  scale_x_continuous(breaks = seq(5, 195, by = 10)) +
  facet_wrap(~ User_Group_Composition, ncol = 1) +
  labs(
    title = "Observation Duration by User Group",
    x = "Duration (sec)", y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "top"
  )

# ---- Histogram of Observation Duration by Park Site ----
avg_med_df <- bird_data %>%
  filter(!is.na(SiteID)) %>%
  group_by(SiteID) %>%
  summarise(
    avg = mean(Observation_Duration, na.rm = TRUE),
    med = median(Observation_Duration, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(bird_data, aes(x = Observation_Duration)) +
  geom_histogram(binwidth = 10, boundary = 5, fill = "#619CFF", color = "white", alpha = 0.8) +
  stat_bin(binwidth = 10, boundary = 5, geom = "text",
           aes(label = after_stat(count)),
           vjust = -0.5, size = 3.2, color = "black") +
  geom_density(aes(y = ..count.. * 10), color = "darkblue", size = 1.2, adjust = 1.1) +
  geom_vline(data = avg_med_df, aes(xintercept = avg, color = "Mean"), linetype = "dashed", linewidth = 1) +
  geom_vline(data = avg_med_df, aes(xintercept = med, color = "Median"), linetype = "dashed", linewidth = 1) +
  geom_text(data = avg_med_df, aes(x = avg, y = 85, label = paste0("Mean: ", round(avg, 1), "s")),
            color = "purple", hjust = -0.1, size = 3.5, inherit.aes = FALSE) +
  geom_text(data = avg_med_df, aes(x = med, y = 75, label = paste0("Med: ", round(med, 1), "s")),
            color = "orange", hjust = -0.1, size = 3.5, inherit.aes = FALSE) +
  scale_color_manual(name = "Reference Line", values = c("Mean" = "purple", "Median" = "orange")) +
  scale_x_continuous(breaks = seq(5, 195, by = 10)) +
  facet_wrap(~ SiteID, ncol = 1) +
  labs(
    title = "Observation Duration by Park Site",
    x = "Duration (sec)", y = "Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "top"
  )

# ---- Box Plot of Observation Duration by User Group ----
plot_df <- bird_data %>%
  mutate(
    User_Type = case_when(
      Total_Humans == 0 ~ "No users",
      Total_Humans > 0 & Total_Dogs == 0 ~ "No dogs",
      Total_Humans > 0 & Total_Dogs > 0 ~ "Dogs",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(User_Type))

plot_df$User_Type <- factor(plot_df$User_Type, levels = c("Dogs", "No dogs", "No users"))
group_colors <- c("Dogs" = "#00BA38", "No dogs" = "#619CFF", "No users" = "#F8766D")

ggplot(plot_df, aes(x = User_Type, y = Observation_Duration, fill = User_Type)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1.5, color = "black") +
  scale_fill_manual(values = group_colors) +
  labs(
    title = "Observation Duration by User Group",
    x = "User Group Composition",
    y = "Observation Duration (seconds)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

# ---- Box Plot of Observation Duration by Park Site ----
plot_df <- bird_data %>%
  filter(!is.na(SiteID), !is.na(Observation_Duration)) %>%
  mutate(SiteID = factor(tolower(SiteID), levels = c("oaks", "sellwood", "smith bybee")))

site_colors <- c("oaks" = "#00BA38", "sellwood" = "#619CFF", "smith bybee" = "#F8766D")

ggplot(plot_df, aes(x = SiteID, y = Observation_Duration, fill = SiteID)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1.5, color = "black") +
  scale_fill_manual(values = site_colors) +
  labs(
    title = "Observation Duration by Park Site",
    x = "Park Site",
    y = "Observation Duration (seconds)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none"
  )

# ---- Stacked Bar Plot of Birdsong Activity by Sites and Species ----
species_map <- c(
  SS = "Song Sparrow", ST = "Spotted Towhee", LG = "Lesser Goldfinch",
  BCC = "Black-capped Chickadee", BW = "Bewick's Wren", GCS = "Golden-crowned Sparrow",
  RCK = "Ruby-crowned Kinglet", Non_SS = "Non Song Sparrow", All = "Any Bird (All)"
)

long_df <- map_dfr(names(species_map), function(sp) {
  df <- bird_data
  singing_col <- paste0("Singing_", sp)
  calling_col <- paste0("Chips_Calls_", sp)
  
  df %>%
    mutate(Status = case_when(
      !!sym(singing_col) == 1 & !!sym(calling_col) == 1 ~ "Both",
      !!sym(singing_col) == 1 ~ "Singing",
      !!sym(calling_col) == 1 ~ "Calling",
      TRUE ~ "Silent"
    )) %>%
    transmute(SiteID, Species = species_map[[sp]], Status)
})

plot_df <- long_df %>%
  count(SiteID, Species, Status) %>%
  group_by(SiteID, Species) %>%
  mutate(
    Percent = n / sum(n),
    Status = factor(Status, levels = c("Silent", "Singing", "Calling", "Both"))
  ) %>%
  ungroup()

species_levels <- unname(species_map)

plot_df <- plot_df %>%
  mutate(Species = factor(Species, levels = species_levels))

ggplot(plot_df, aes(x = SiteID, y = Percent, fill = Status)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  facet_wrap(~ Species, ncol = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(
    Silent = "#F8766D",
    Singing = "#00BA38",
    Calling = "#619CFF",
    Both = "#AA4FCB"
  )) +
  labs(title = "Birdsong Activity by Sites and Species", x = "Site", y = "Proportion", fill = "Activity") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10), legend.position = "right")

# ---- Bar Plot of Song Sparrow Activity by Park Site and User Group ----
ggplot(
  bird_data %>%
    filter(!User_Group_Composition %in% c("Other", NA)) %>%
    mutate(
      Birdsong_Status_SS = case_when(
        Singing_SS == 1 & Chips_Calls_SS == 1 ~ "Both",
        Singing_SS == 1 ~ "Singing",
        Chips_Calls_SS == 1 ~ "Calling",
        TRUE ~ "Silent"
      ),
      Birdsong_Status_SS = factor(Birdsong_Status_SS, levels = c("Silent", "Singing", "Calling", "Both"))
    ) %>%
    count(SiteID, User_Group_Composition, Birdsong_Status_SS),
  aes(x = Birdsong_Status_SS, y = n, fill = Birdsong_Status_SS)
) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  facet_grid(SiteID ~ User_Group_Composition) +
  labs(title = "Song Sparrow Activity by User Group and Park Site",
       x = "Birdsong Activity", y = "Count", fill = "Activity") +
  scale_fill_manual(values = c(
    Silent = "#F8766D",
    Singing = "#00BA38",
    Calling = "#619CFF",
    Both = "#AA4FCB"
  ),
  breaks = c("Silent", "Singing", "Calling", "Both")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

# ---- Time Series Plot of Song Sparrow with Human and Dog Noise Bands ----
plot_data <- bird_data %>%
  mutate(
    Birdsong_Status_SS = case_when(
      Singing_SS == 1 & Chips_Calls_SS == 1 ~ "Both",
      Singing_SS == 1 ~ "Singing",
      Chips_Calls_SS == 1 ~ "Calling",
      TRUE ~ "Silent"
    ),
    Birdsong_Status_SS = replace_na(Birdsong_Status_SS, "Gap"),
    Birdsong_Status_SS = factor(Birdsong_Status_SS, levels = c("Silent", "Singing", "Calling", "Both", "Gap"))
  ) %>%
  filter(!is.na(Hour), !is.na(End_Hour))

obs_order <- plot_data %>%
  distinct(SiteID, Date_Parsed, Obs_Label) %>%
  arrange(factor(SiteID, levels = c("oaks", "sellwood", "smith bybee")), Date_Parsed) %>%
  pull(Obs_Label)

plot_data <- plot_data %>%
  mutate(Obs_Label = factor(Obs_Label, levels = rev(obs_order)))

obs_segments <- plot_data %>%
  select(Obs_Label, Hour, End_Hour, Birdsong_Status_SS)

gap_segments <- plot_data %>%
  arrange(Obs_Label, Hour) %>%
  group_by(Obs_Label) %>%
  mutate(Next_Hour = lead(Hour), Prev_End = End_Hour) %>%
  filter(!is.na(Next_Hour)) %>%
  mutate(Gap_Start = Prev_End, Gap_End = Next_Hour) %>%
  filter(Gap_End > Gap_Start) %>%
  transmute(
    Obs_Label,
    Hour = Gap_Start,
    End_Hour = Gap_End,
    Birdsong_Status_SS = factor("Gap", levels = c("Silent", "Singing", "Calling", "Both", "Gap"))
  )

combined <- bind_rows(obs_segments, gap_segments)

human_noise_bands <- bird_data %>%
  filter(!is.na(Human_Noise_Max), Total_Humans > 0, Obs_Label %in% obs_order) %>%
  mutate(Obs_Label = factor(Obs_Label, levels = rev(obs_order))) %>%
  mutate(y_numeric = as.numeric(Obs_Label)) %>%
  transmute(
    ymin = y_numeric + 0.1,
    ymax = y_numeric + 0.4,
    xmin = Hour,
    xmax = End_Hour,
    Noise = Human_Noise_Max
  )

dog_noise_bands <- bird_data %>%
  filter(!is.na(Dog_Noise_Max), Total_Dogs > 0, Obs_Label %in% obs_order) %>%
  mutate(Obs_Label = factor(Obs_Label, levels = rev(obs_order))) %>%
  mutate(y_numeric = as.numeric(Obs_Label)) %>%
  transmute(
    ymin = y_numeric - 0.4,
    ymax = y_numeric - 0.1,
    xmin = Hour,
    xmax = End_Hour,
    Noise = Dog_Noise_Max
  )

status_colors <- c(
  Silent = "#F8766D",
  Singing = "#00BA38",
  Calling = "#619CFF",
  Both = "#AA4FCB",
  Gap = "gray90"
)

ggplot() +
  geom_rect(data = human_noise_bands,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Noise),
            color = "white", alpha = 0.8) +
  scale_fill_gradient(low = "gold1", high = "gold4", name = "Human Noise Level") +
  new_scale_fill() +
  geom_rect(data = dog_noise_bands,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Noise),
            color = "white", alpha = 0.8) +
  scale_fill_gradient(low = "firebrick1", high = "firebrick4", name = "Dog Noise Level") +
  geom_segment(data = combined,
               aes(x = Hour, xend = End_Hour, y = as.numeric(Obs_Label), yend = as.numeric(Obs_Label),
                   color = Birdsong_Status_SS),
               size = 4) +
  scale_color_manual(values = status_colors) +
  scale_y_continuous(
    breaks = seq_along(rev(obs_order)),
    labels = rev(obs_order),
    name = "Site (Date)"
  ) +
  scale_x_continuous(
    name = "Time of Day", breaks = seq(8, 10, by = 1/6),
    labels = function(x) {
      h <- floor(x)
      m <- round((x - h) * 60)
      sprintf("%02d:%02d", h, m)
    }
  ) +
  labs(title = "Song Sparrow Activity with Human and Dog Noise Bands",
       x = "Time of Day", color = "Birdsong Activity") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 7)
  )

# ---- Time Series Plot of Song Sparrow by Site and Date ----
plot_song_sparrow_day <- function(bird_data, site_filter, date_filter) {
  plot_df <- bird_data %>%
    filter(SiteID == site_filter, Date_Parsed == ymd(date_filter)) %>%
    mutate(
      Dog_Noise_Level = Dog_Noise_Max,
      Human_Noise_Level = Human_Noise_Max
    )
  
  if (nrow(plot_df) == 0) {
    return(ggplot() +
             theme_void() +
             labs(title = paste(site_filter, "-", format(ymd(date_filter), "%m/%d/%Y"), "\nNo Data")) +
             theme(plot.title = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray50")))
  }
  
  singing <- plot_df %>% filter(Singing_SS == 1)
  singing_stop <- plot_df %>% filter(Singing_Cessation_SS == 1)
  calling <- plot_df %>% filter(Chips_Calls_SS == 1)
  calling_stop <- plot_df %>% filter(Chip_Call_Cessation_SS == 1)
  dogs <- plot_df %>% filter(Total_Dogs > 0)
  humans <- plot_df %>% filter(Total_Humans > 0)
  all_obs <- plot_df
  
  ggplot() +
    geom_segment(data = all_obs, aes(x = Hour, xend = End_Hour, y = "Observation Duration", yend = "Observation Duration"),
                 color = "black", size = 3) +
    geom_segment(data = dogs,
                 aes(x = Hour, xend = End_Hour, y = "Dog Presence", yend = "Dog Presence", alpha = Dog_Noise_Level),
                 color = "#FF0000", size = 3) +
    geom_segment(data = humans,
                 aes(x = Hour, xend = End_Hour, y = "Human Presence", yend = "Human Presence", alpha = Human_Noise_Level),
                 color = "#FF0000", size = 3) +
    geom_segment(data = calling,
                 aes(x = Hour, xend = End_Hour, y = "Calling", yend = "Calling"),
                 color = "#1E90FF", size = 3) +
    geom_segment(data = calling_stop,
                 aes(x = Hour, xend = End_Hour, y = "Calling Cessation", yend = "Calling Cessation"),
                 color = "#1E90FF", size = 3) +
    geom_segment(data = singing,
                 aes(x = Hour, xend = End_Hour, y = "Singing", yend = "Singing"),
                 color = "#32CD32", size = 3) +
    geom_segment(data = singing_stop,
                 aes(x = Hour, xend = End_Hour, y = "Singing Cessation", yend = "Singing Cessation"),
                 color = "#32CD32", size = 3) +
    scale_y_discrete(limits = rev(c("Calling", "Calling Cessation", "Singing", "Singing Cessation",
                                    "Dog Presence", "Human Presence", "Observation Duration"))) +
    scale_alpha_continuous(range = c(0.3, 1), name = "Noise Level") +
    scale_x_continuous(
      name = "Pacific Time",
      breaks = seq(8, 10, by = 1/6),
      labels = function(x) {
        h <- floor(x)
        m <- round((x - h) * 60)
        sprintf("%02d:%02d", h, m)
      }
    ) +
    labs(
      title = paste0(str_to_title(site_filter), " (", format(ymd(date_filter), "%m/%d/%Y"), ")"),
      y = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    )
}

plot_site_overview_auto <- function(bird_data, site_filter, save_path = NULL) {
  site_filter <- tolower(site_filter)
  
  available_dates <- bird_data %>%
    filter(SiteID == site_filter) %>%
    distinct(Date_Parsed) %>%
    arrange(Date_Parsed) %>%
    pull(Date_Parsed) %>%
    tail(4)
  
  plots <- lapply(available_dates, function(date) {
    plot_song_sparrow_day(bird_data, site_filter, date)
  })
  
  title_text <- paste0(str_to_title(site_filter), " - Song Sparrow Activity")
  
  final_plot <- gridExtra::grid.arrange(grobs = plots, nrow = 2, ncol = 2,
                                        top = title_text)
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = final_plot, width = 14, height = 10, dpi = 300)
  }
  
  invisible(final_plot)
}

print(plot_site_overview_auto(bird_data, site_filter = "oaks"))
print(plot_site_overview_auto(bird_data, site_filter = "sellwood"))
print(plot_site_overview_auto(bird_data, site_filter = "smith bybee"))

# ---- Radar Plot of Birdsong Activity by Sites and Species ----
make_radar_plot <- function(type = c("Singing", "Calling", "Vocal"), title_text = "") {
  type <- match.arg(type)
  
  species_map <- c(
    SS = "Song Sparrow",
    ST = "Spotted Towhee",
    LG = "Lesser Goldfinch",
    BCC = "Chickadee",
    BW = "Bewick's Wren",
    GCS = "Golden-crowned Sparrow",
    RCK = "Ruby-crowned Kinglet"
  )
  
  if (type == "Singing") {
    col_prefix <- "Singing_"
  } else if (type == "Calling") {
    col_prefix <- "Chips_Calls_"
  } else {
    vocal_data <- map_dfr(names(species_map), function(code) {
      singing_col <- paste0("Singing_", code)
      calling_col <- paste0("Chips_Calls_", code)
      bird_data %>%
        group_by(SiteID) %>%
        summarise(n = sum(.data[[singing_col]] + .data[[calling_col]], na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = SiteID, values_from = n) %>%
        mutate(Species = species_map[[code]])
    })
  }
  
  if (type %in% c("Singing", "Calling")) {
    vocal_data <- map_dfr(names(species_map), function(code) {
      vocal_col <- paste0(col_prefix, code)
      bird_data %>%
        group_by(SiteID) %>%
        summarise(n = sum(.data[[vocal_col]], na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = SiteID, values_from = n) %>%
        mutate(Species = species_map[[code]])
    })
  }
  
  vocal_data[is.na(vocal_data)] <- 0
  vocal_data <- vocal_data %>% relocate(Species)
  colnames(vocal_data)[-1] <- str_to_title(colnames(vocal_data)[-1])
  
  vocal_data_scaled <- vocal_data %>%
    mutate(across(-Species, scales::rescale)) %>%
    rename(group = Species)
  
  palette_colors <- c(
    "Song Sparrow" = "#E60000",
    "Spotted Towhee" = "#002FA7",
    "Lesser Goldfinch" = "#4DAF4A",
    "Chickadee" = "#AA4FCB",
    "Bewick's Wren" = "#FF7F00",
    "Golden-crowned Sparrow" = "#1E90FF",
    "Ruby-crowned Kinglet" = "#5E0004"
  )
  
  radar_plot <- ggradar(vocal_data_scaled,
                        grid.min = 0, grid.mid = 0.5, grid.max = 1,
                        values.radar = c("0%", "50%", "100%"),
                        group.line.width = 1.2,
                        group.point.size = 3,
                        legend.position = "right",
                        background.circle.colour = "gray95",
                        gridline.mid.colour = "gray80",
                        font.radar = "Arial") +
    scale_color_manual(values = palette_colors) +
    scale_fill_manual(values = palette_colors) +
    ggtitle(title_text)
  
  long_data <- vocal_data %>%
    pivot_longer(-Species, names_to = "Site", values_to = "Value")
  
  scaled_long <- vocal_data_scaled %>%
    rename(Species = group) %>%
    pivot_longer(-Species, names_to = "Site", values_to = "Scaled")
  
  label_data <- left_join(scaled_long, long_data, by = c("Species", "Site"))
  
  n_sites <- length(unique(label_data$Site))
  site_order <- sort(unique(label_data$Site))
  label_data <- label_data %>%
    mutate(
      Site = factor(Site, levels = site_order),
      angle = 2 * pi * (as.numeric(Site) - 1) / n_sites,
      x = 1 * Scaled * sin(angle),
      y = 1 * Scaled * cos(angle)
    )
  
  radar_plot + 
    geom_text(data = label_data, 
              aes(x = x, y = y, label = Value, color = Species, group = Species),
              size = 3.6, fontface = "bold", show.legend = FALSE)
}

make_radar_plot("Vocal", "Radar Plot of Total Vocal Activity by Sites and Species")
make_radar_plot("Singing", "Radar Plot of Singing Activity by Sites and Species")
make_radar_plot("Calling", "Radar Plot of Calling Activity by Sites and Species")
