
library(tidyverse)   
library(gridExtra)   
library(GGally)      
library(reshape2)    
library(fmsb)       


p1 <- read_csv("p1_csv.csv")
p2 <- read_csv("p2_csv.csv")

p1 <- p1 %>% mutate(Playlist = "P1")
p2 <- p2 %>% mutate(Playlist = "P2")

df_combined <- bind_rows(p1, p2)

str(df_combined)

summary_p1 <- p1 %>% summarise(
  BPM_avg       = mean(BPM, na.rm = TRUE),
  BPM_sd        = sd(BPM, na.rm = TRUE),
  Energy_avg    = mean(Energy, na.rm = TRUE),
  Dance_avg     = mean(Dance, na.rm = TRUE),
  Loud_avg      = mean(Loud, na.rm = TRUE),
  Valence_avg   = mean(Valence, na.rm = TRUE),
  Acoustic_avg  = mean(Acoustic, na.rm = TRUE),
  Popular_avg   = mean(Popularity, na.rm = TRUE)
)

summary_p2 <- p2 %>% summarise(
  BPM_avg       = mean(BPM, na.rm = TRUE),
  BPM_sd        = sd(BPM, na.rm = TRUE),
  Energy_avg    = mean(Energy, na.rm = TRUE),
  Dance_avg     = mean(Dance, na.rm = TRUE),
  Loud_avg      = mean(Loud, na.rm = TRUE),
  Valence_avg   = mean(Valence, na.rm = TRUE),
  Acoustic_avg  = mean(Acoustic, na.rm = TRUE),
  Popular_avg   = mean(Popularity, na.rm = TRUE)
)

summary_p1
summary_p2

all_summaries <- bind_rows(
  summary_p1 %>% mutate(Playlist = "P1"),
  summary_p2 %>% mutate(Playlist = "P2")
)
all_summaries

plot_density_p1 <- ggplot(p1, aes(x = BPM)) +
  geom_density(fill = "cyan", alpha = 0.3) +
  labs(title = "P1 BPM Density", x = "BPM", y = "Density") +
  theme_minimal()

plot_density_p2 <- ggplot(p2, aes(x = BPM)) +
  geom_density(fill = "red", alpha = 0.3) +
  labs(title = "P2 BPM Density", x = "BPM", y = "Density") +
  theme_minimal()

plot_density_combined <- ggplot(df_combined, aes(x = BPM, fill = Playlist)) +
  geom_density(alpha = 0.3) +
  labs(title = "Combined BPM Density", x = "BPM", y = "Density") +
  scale_fill_manual(values = c("P1" = "cyan", "P2" = "red")) +
  theme_minimal()

plot_box_BPM <- ggplot(df_combined, aes(x = Playlist, y = BPM, fill = Playlist)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "BPM Distribution by Playlist", x = NULL, y = "BPM") +
  scale_fill_manual(values = c("P1" = "cyan", "P2" = "red")) +
  theme_minimal()

plot_box_Energy <- ggplot(df_combined, aes(x = Playlist, y = Energy, fill = Playlist)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Energy Distribution by Playlist", x = NULL, y = "Energy") +
  scale_fill_manual(values = c("P1" = "cyan", "P2" = "red")) +
  theme_minimal()



BPMDiff      = mean(p2[["BPM"]])       - mean(p1[["BPM"]])
EnergyDiff   = mean(p2[["Energy"]])    - mean(p1[["Energy"]])
DanceDiff    = mean(p2[["Dance"]])     - mean(p1[["Dance"]])
LoudDiff     = mean(p2[["Loud"]])      - mean(p1[["Loud"]])
ValenceDiff  = mean(p2[["Valence"]])   - mean(p1[["Valence"]])
AcousticDiff = mean(p2[["Acoustic"]])  - mean(p1[["Acoustic"]])
PopDiff      = mean(p2[["Popularity"]]) - mean(p1[["Popularity"]])

features <- c("BPM", "Energy", "Dance", "Loud", "Valence", "Acoustic", "Popularity")
values   <- c(BPMDiff, EnergyDiff, DanceDiff, LoudDiff, ValenceDiff, AcousticDiff, PopDiff)
diff.data <- data.frame(Feature = features, MeanDifference = values)

plot_diffs <- ggplot(diff.data, aes(x = reorder(Feature, MeanDifference), y = MeanDifference, fill = MeanDifference > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  labs(title = "Mean Differences (P2 - P1)", x = NULL, y = "Difference") +
  theme_minimal()



ttest_bpm <- t.test(BPM ~ Playlist, data = df_combined)
ttest_energy <- t.test(Energy ~ Playlist, data = df_combined)

ttest_bpm
ttest_energy


numeric_cols <- df_combined %>%
  select(BPM, Energy, Dance, Loud, Valence, Acoustic, Popularity)

corr_plot <- ggcorr(numeric_cols, nbreaks = 5, label = TRUE)

radar_data <- df_combined %>%
  group_by(Playlist) %>%
  summarise(
    BPM       = mean(BPM, na.rm = TRUE),
    Energy    = mean(Energy, na.rm = TRUE),
    Dance     = mean(Dance, na.rm = TRUE),
    Loud      = mean(Loud, na.rm = TRUE),
    Valence   = mean(Valence, na.rm = TRUE),
    Acoustic  = mean(Acoustic, na.rm = TRUE),
    Popular   = mean(Popularity, na.rm = TRUE)
  )


max_vals <- apply(radar_data[, -1], 2, max) * 1.1
min_vals <- apply(radar_data[, -1], 2, min) * 0.9

radar_data_fmsb <- rbind(max_vals, min_vals, radar_data[, -1])
radar_data_fmsb <- as.data.frame(radar_data_fmsb)

par(mfrow = c(1,2)) 
radarchart(radar_data_fmsb[c(1,2,3), ], axistype = 1,
           title = "P1 Radar Chart",
           pcol = "cyan", pfcol = rgb(0,1,1,0.3), plwd=2)

radarchart(radar_data_fmsb[c(1,2,4), ], axistype = 1,
           title = "P2 Radar Chart",
           pcol = "red", pfcol = rgb(1,0,0,0.3), plwd=2)

par(mfrow = c(1,1))

grid.arrange(
  plot_density_p1,
  plot_density_p2,
  plot_density_combined,
  plot_diffs,
  ncol = 2,
  top = "Spotify Analysis - Density & Mean Difference Plots"
)
