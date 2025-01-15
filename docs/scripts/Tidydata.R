source("scripts/Webscraping.R")
#
library(tidyverse)
library(scales)
#
#data <- get_data(4)
#games_left <- get_rest_games()
#

#
#write_csv(data, file = "data/raw.csv")
#write_csv(games_left, file = "data/games_left.csv")
data <- read_csv("data/raw.csv")
games_left <- read_csv("data/games_left.csv")
#
data$O <- as.numeric(data$O)
data$M <- as.numeric(data$M)
data$S <- as.numeric(data$S)
data$P <- as.numeric(data$P)
data$R <- as.numeric(data$R)
data$L <- as.numeric(data$L)
data$`L%` <- as.numeric(data$`L%`)
data$`+` <- as.numeric(data$`+`)
data$`-` <- as.numeric(data$`-`)
data$`+/-` <- as.numeric(data$`+/-`)
#
#There are two same name for to different players - ID would be nice
data %>% group_by(PELAAJA, kausi) %>% count() %>% arrange(desc(n)) %>% head(5)
#

data <- data %>% mutate(aloitusvuosi = as.integer(str_sub(kausi,1,4)),
                          viimeinenvuosi = as.integer(str_sub(kausi,6,9)))
#
data2 <- data  %>%  group_by(PELAAJA) %>% mutate(kaudet = n())
data3 <- data2 %>% filter(kausi == "2024-2025")

pelaajat_yv <-
  data3 %>%
  group_by(PELAAJA) %>%
  summarise(
    PELAAJA = first(PELAAJA),
    JOUKKUE = first(JOUKKUE),
    aloitus = min(aloitusvuosi),
    viimeinen = max(viimeinenvuosi),
    ura =  paste0(aloitus, "–", ifelse(viimeinen == max(data2$viimeinenvuosi), yes = "", no = viimeinen)),
    O = sum(O), 
    L = sum(L),
    M = sum(M),
    "M/O" = M / O,
    "L/O" = L / O,
    raw = M / L
  )
#
pelaajat_yv_25_all <-
  data2 %>%
  group_by(PELAAJA) %>%
  summarise(
    PELAAJA = first(PELAAJA),
    aloitus = min(aloitusvuosi),
    viimeinen = max(viimeinenvuosi),
    ura =  paste0(aloitus, "–", ifelse(viimeinen == max(data2$viimeinenvuosi), yes = "", no = viimeinen)),
    O = sum(O), 
    L = sum(L),
    M = sum(M),
    "M/O" = M / O,
    "L/O" = L / O,
    raw = M / L
  )

pelaajat_yv_25 <- pelaajat_yv %>% filter(viimeinen==2025, L >0)
pelaajat_yv_25 <- pelaajat_yv_25 %>% select(-aloitus,-ura)
#
#
raw_limit_09 <- quantile(pelaajat_yv_25 %>% pull(raw),0.9, na.rm=T)
raw_limit_07 <- quantile(pelaajat_yv_25 %>% pull(raw),0.7, na.rm=T)
raw_limit_05 <- quantile(pelaajat_yv_25 %>% pull(raw),0.5, na.rm=T)
raw_limit_03 <- quantile(pelaajat_yv_25 %>% pull(raw),0.3, na.rm=T)
#
lo_limit_09 <- quantile(pelaajat_yv_25 %>% pull(`L/O`),0.9, na.rm=T)
lo_limit_07 <- quantile(pelaajat_yv_25 %>% pull(`L/O`),0.7, na.rm=T)
lo_limit_05 <- quantile(pelaajat_yv_25 %>% pull(`L/O`),0.5, na.rm=T)
lo_limit_03 <- quantile(pelaajat_yv_25 %>% pull(`L/O`),0.3, na.rm=T)
#



pelaajat_yv_25 <-  pelaajat_yv_25 %>% mutate(raw_luokka =  case_when(raw > raw_limit_09~ "Very High,%",
                                                                     raw > raw_limit_07 ~ "High,%", 
                                                                     raw > raw_limit_05 ~ "Average High,%", 
                                                                     raw > raw_limit_03 ~ "Average Low,%", 
                                                                     raw <= raw_limit_03 ~ "Low,%"))
#
pelaajat_yv_25 <-  pelaajat_yv_25 %>% mutate("L/O_luokka" =  case_when(`L/O` > lo_limit_09 ~ "Very High,spg",
                                                                       `L/O` > lo_limit_07 ~ "High,spg", 
                                                                       `L/O` > lo_limit_05 ~ "Average High,spg",
                                                                       `L/O` > lo_limit_03 ~ "Average Low,spg", 
                                                                       `L/O` <=  lo_limit_03 ~ "Low,spg"))
#
pelaajat_yv_25$raw_luokka <- factor(pelaajat_yv_25$raw_luokka, levels = c("Low,%","Average Low,%","Average High,%", "High,%", "Very High,%"))
#
pelaajat_yv_25$`L/O_luokka` <- factor(pelaajat_yv_25$`L/O_luokka`, levels = c("Low,spg","Average Low,spg","Average High,spg", "High,spg", "Very High,spg"))
#

#Let us calculate means etc only for player with more than 10 shots. this is based on this season
mean_x <- mean(pelaajat_yv_25 %>% filter(L > 10) %>% pull(raw))
#median_x <- median(pelaajat_yv_25_all %>% filter(L > 10) %>% pull(raw))
var_x <- var(pelaajat_yv_25 %>% filter(L > 10) %>% pull(raw))

# Estimate Beta parameters using Method of Moments
alpha <- mean_x * ((mean_x * (1 - mean_x) / var_x) - 1)
beta <- (1 - mean_x) * ((mean_x * (1 - mean_x) / var_x) - 1)
#
plot_rawhist_vs_beta <- pelaajat_yv_25 %>%
  filter(L > 15) %>%
  ggplot() +
  geom_histogram(aes(raw, y = ..density..), binwidth = 0.01) +
  stat_function(fun = function(x) dbeta(x, alpha, beta), color = "black",
                size = 1) +
  xlab("Raw Shooting Percentage") +
  theme_minimal()
#
#ggsave("images/rawhist_vs_beta.jpg", plot = plot_rawhist_vs_beta, width = 8, height = 8, dpi = 300)
#
pelaajat_yv_25 <- pelaajat_yv_25 %>%
  mutate(eb_raw = (M + alpha) / (L + alpha + beta), alhpa_new = M+ alpha, beta_new = L + alpha + beta)
#
eb_limit_09 <- quantile(pelaajat_yv_25 %>% pull(eb_raw),0.9, na.rm=T)
eb_limit_07 <- quantile(pelaajat_yv_25 %>% pull(eb_raw),0.7, na.rm=T)
eb_limit_05 <- quantile(pelaajat_yv_25 %>% pull(eb_raw),0.5, na.rm=T)
eb_limit_03 <- quantile(pelaajat_yv_25 %>% pull(eb_raw),0.3, na.rm=T)
#
pelaajat_yv_25 <-  pelaajat_yv_25 %>% mutate(eb_raw_luokka =  case_when(eb_raw > eb_limit_09~ "Very High,%",
                                                                        eb_raw > eb_limit_07 ~ "High,%", 
                                                                        eb_raw > eb_limit_05 ~ "Average High,%", 
                                                                        eb_raw > eb_limit_03 ~ "Average Low,%", 
                                                                        eb_raw <= eb_limit_03 ~ "Low,%"))
#
pelaajat_yv_25$eb_raw_luokka <- factor(pelaajat_yv_25$eb_raw_luokka, levels = c("Low,%","Average Low,%","Average High,%", "High,%", "Very High,%"))
#
pelaajat_yv_25 <- pelaajat_yv_25 %>% unite("yhd_luokka_eb",eb_raw_luokka, `L/O_luokka`, sep = " -- ", remove = FALSE)
#

#
pelaajat_yv_25 %>% count(`L/O_luokka`,raw_luokka)
summary_table <-pelaajat_yv_25 %>% count(`L/O_luokka`,eb_raw_luokka) %>% pivot_wider(names_from = eb_raw_luokka, values_from = n)
#
#
xxx_pelaajat <- pelaajat_yv_25 %>% filter(eb_raw_luokka=="Low,%", `L/O_luokka` %in% c("Very High,spg")) %>% arrange(desc(L)) %>% head(5) %>%
  select(PELAAJA,JOUKKUE,O,L,M,`M/O`,`L/O`,raw,eb_raw,yhd_luokka_eb) %>% rename("Adjusted" = eb_raw)
xxx_pelaajat2 <- pelaajat_yv_25 %>% filter(eb_raw_luokka=="Very High,%", `L/O_luokka` %in% c("Low,spg"))  %>% arrange(desc(L)) %>% head(5) %>%
  select(PELAAJA,JOUKKUE,O,L,M,`M/O`,`L/O`,raw,eb_raw,yhd_luokka_eb) %>% rename("Adjusted" = eb_raw)
xxx_pelaajat3 <- pelaajat_yv_25 %>% filter(eb_raw_luokka=="Very High,%", `L/O_luokka` == "Very High,spg")  %>% arrange(desc(L)) %>% head(5) %>%
  select(PELAAJA,JOUKKUE,O,L,M,`M/O`,`L/O`,raw,eb_raw,yhd_luokka_eb) %>% rename("Adjusted" = eb_raw)
#
#write_csv(xxx_pelaajat, file = "data/players_low_eb_veryhigh_spg.csv")
#write_csv(xxx_pelaajat2, file = "data/players_veryhigh_eb_low_spg.csv")
#write_csv(xxx_pelaajat3, file = "data/players_veryhigh_eb_veryhigh_spg.csv")
#
top_10_eb <- pelaajat_yv_25 %>% arrange(desc(eb_raw)) %>% head(10)
top_10_raw <- pelaajat_yv_25 %>% arrange(desc(raw)) %>% head(10)

plot_segments <- pelaajat_yv_25 %>% ggplot(aes(x= `L/O`, eb_raw)) +
  geom_rect(aes(xmin=lo_limit_09, xmax = Inf, ymin=eb_limit_09, ymax =Inf), fill="lightgreen") +
  geom_rect(aes(xmin=lo_limit_09, xmax = Inf, ymin=0, ymax =eb_limit_03), fill="red") +
  geom_point() +
  geom_vline(xintercept=lo_limit_03) +
  geom_vline(xintercept=lo_limit_05) +
  geom_vline(xintercept=lo_limit_07) +
  geom_vline(xintercept=lo_limit_09) +
  geom_hline(yintercept=eb_limit_03) +
  geom_hline(yintercept=eb_limit_05) +
  geom_hline(yintercept=eb_limit_07) +
  geom_hline(yintercept=eb_limit_09) +
  scale_x_continuous(breaks = seq(0,12,1)) +
  labs(title = "Adjusted Shooting Percentage vs. Shoots Per Game", x= "Shoots Per Game", y="Adjusted") +
  theme_minimal()
#
#ggsave("images/segments.jpg", plot = plot_segments, width = 8, height = 8, dpi = 300)
#
#pelaajat_yv_25 %>% ggplot(aes(x= `L/O`, eb_raw)) +
#  geom_point()
#
#pelaajat_yv_25 %>% ggplot(aes(x= raw, eb_raw)) +
#  geom_point()

#
summary_top_10_eb <-top_10_eb %>%  mutate("Rank" = rank(desc(eb_raw))) %>% select(PELAAJA,JOUKKUE,O,L,M,`M/O`,`L/O`,raw,eb_raw,Rank) %>% rename("Adjusted" = eb_raw)
summary_top_10_raw <-top_10_raw %>%  mutate("Rank" = rank(desc(raw))) %>% select(PELAAJA,JOUKKUE,O,L,M,`M/O`,`L/O`,raw,eb_raw,Rank) %>% rename("Adjusted" = eb_raw)
#
#Let us write Toptens and summary to folder named data
#write_csv(summary_table, file = "data/summarytable.csv")
#write_csv(summary_top_10_eb, file = "data/summary_top_10_eb.csv")
#write_csv(summary_top_10_raw, file = "data/summary_top_10_raw.csv")
#
plot_top_10_eb <- ggplot(summary_top_10_eb, aes(x = Adjusted, y = reorder(PELAAJA, -Rank))) +
  geom_point(size = 3) +  
  geom_point(aes(x = raw, y = reorder(PELAAJA, -Rank)),shape = 5) + 
  geom_segment(aes(x = 0.00, xend = Adjusted, 
                   y = reorder(PELAAJA, -Rank), 
                   yend = reorder(PELAAJA, -Rank)), 
               color = "black", size = 0.3) + 
  geom_segment(aes(x = 0.00, xend = raw, 
                   y = reorder(PELAAJA, -Rank), 
                   yend = reorder(PELAAJA, -Rank)), 
               color = "black", size = 0.3,linetype = "dotted") +  
  scale_x_continuous(breaks = seq(0,0.35, 0.025)) +
  labs(
    title = "Top-10 Adjusted Shooting Percentage in F-liiga 2024-2025",
    #subtitle = "Dotted lines are raw percentages",
    x = "Shooting Percentage",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  
    axis.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 9, hjust = 1)
  )
#
plot_top_10_raw <- ggplot(summary_top_10_raw, aes(x = Adjusted, y = reorder(PELAAJA, -Rank))) +
  geom_point(size = 3) +  # Plot the points
  geom_point(aes(x = raw, y = reorder(PELAAJA, -Rank)),shape = 5) + 
  geom_segment(aes(x = 0.00, xend = Adjusted, 
                   y = reorder(PELAAJA, -Rank), 
                   yend = reorder(PELAAJA, -Rank)), 
               color = "black", size = 0.3) +  
  geom_segment(aes(x = 0.00, xend = raw, 
                   y = reorder(PELAAJA, -Rank), 
                   yend = reorder(PELAAJA, -Rank)), 
               color = "black", size = 0.3,linetype = "dotted") +  
  scale_x_continuous(breaks = seq(0,1, 0.05)) +
  labs(
    title = "Top-10 Raw Shooting Percentage in F-liiga 2024-2025",
    #subtitle = "Black lines are adjusted percentages",
    x = "Shooting Percentage",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  
    axis.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 9, hjust = 1)
  )

#Now we can save plots
#ggsave("images/topten_eb.jpg", plot = plot_top_10_eb, width = 12, height = 8, dpi = 300)
#ggsave("images/topten_raw.jpg", plot = plot_top_10_raw, width = 12, height = 8, dpi = 300)


#
games_left$JOUKKUE <- str_replace(games_left$JOUKKUE, "Nokian KrP", "KrP")
games_left$JOUKKUE <- str_replace(games_left$JOUKKUE, "FBC Turku", "FBC")
games_left$JOUKKUE <- str_replace(games_left$JOUKKUE, "EräViikingit", "ErVi")
pelaajat_yv_25 <- left_join(pelaajat_yv_25, games_left %>% select(JOUKKUE,`Games Left`))
#
simulate_player_goals <- function(alpha, beta, shots_per_game, num_games, num_simulations = 1000) {
  replicate(num_simulations, {
    total_goals <- sum(sapply(1:num_games, function(x) {
      theta <- rbeta(1, alpha, beta)  
      shots <- rpois(1, lambda = shots_per_game)  
      rbinom(1, size = shots, prob = theta)  
    }))
    return(total_goals)
  })
}
#
pelaajat_yv_25$Simulated_Goals <- lapply(1:nrow(pelaajat_yv_25), function(i) {
  simulate_player_goals(
    alpha = pelaajat_yv_25$alhpa_new[i],
    beta = pelaajat_yv_25$beta_new[i],
    shots_per_game = pelaajat_yv_25$`L/O`[i],
    num_games = pelaajat_yv_25$`Games Left`[i]
  )
})


pelaajat_yv_25$Mean_Goals <- sapply(pelaajat_yv_25$Simulated_Goals, mean) 
pelaajat_yv_25$CI_Lower <- sapply(pelaajat_yv_25$Simulated_Goals, function(sim) quantile(sim, 0.1))  
pelaajat_yv_25$CI_Upper <- sapply(pelaajat_yv_25$Simulated_Goals, function(sim) quantile(sim, 0.9))  
#
pelaajat_yv_25$Final_Goals <- pelaajat_yv_25$M+pelaajat_yv_25$Mean_Goals
pelaajat_yv_25$Final_CI_Lower <- pelaajat_yv_25$M+pelaajat_yv_25$CI_Lower
pelaajat_yv_25$Final_CI_Upper <- pelaajat_yv_25$M+pelaajat_yv_25$CI_Upper

#

top_10_final_goals <- pelaajat_yv_25 %>% arrange(desc(Final_Goals)) %>% head(10) %>% 
                                          select(PELAAJA,JOUKKUE,O,L,M,`M/O`,`L/O`,raw,eb_raw,alhpa_new,beta_new,`Games Left`,CI_Lower,
                                                    CI_Upper,Final_Goals,Final_CI_Lower,Final_CI_Upper) %>% rename("Adjusted" = eb_raw)

  #
#write_csv(top_10_final_goals, file = "data/summary_top_10_final_goals.csv")
#
players_2425_summary <- pelaajat_yv_25 %>% select(-Simulated_Goals)
#write_csv(players_2425_summary, file="data/players_2425_summary.csv")
#
plot_top_10_final_goals <- ggplot(top_10_final_goals, aes(x = reorder(PELAAJA, Final_Goals))) +
  geom_segment(aes(
    x = PELAAJA,
    xend = PELAAJA,
    y = 0,
    yend = M
  ), color = "black", size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(
    x = PELAAJA,
    xend = PELAAJA,
    y = Final_CI_Lower,
    yend = Final_CI_Upper
  ), color = "gray", size = 1) +
  
  geom_point(aes(y = Final_Goals), color = "black", size = 2, shape= 5) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  labs(
    title = "F-liiga Prediction: Top-10 Goal Scorers End of Season 2024-2025",
    x = "Player",
    y = "Predicted Goals") +
  theme_minimal() +
  coord_flip()
#
#ggsave("images/topten_final_goals.jpg", plot = plot_top_10_final_goals, width = 8, height = 8, dpi = 300)




