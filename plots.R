source("data.R")

#  PPM OVER TIME, BY YQ AND OP, SEEING SEASONAL TRENDS ACROSS OPS 
seasonal_months <- c("2021 Q3", "2022 Q1", "2022 Q3", "2023 Q1", "2023 Q3", "2024 Q1", "2024 Q3")
seasonal_box <- data.frame(xmin = seq(min(trains$yQ) + 2/4, max(trains$yQ) - 1/4, by = 1),
                           xmax = seq(min(trains$yQ) + 1, max(trains$yQ) + 1/4, by = 1)) %>% 
  mutate(xmax = if_else(xmax <= max(trains$yQ), xmax, xmax -1/4))

##############################################################################
##############################################################################

# FIGURE 1

trains %>% 
  group_by(yQ, operator) %>% 
  summarise(ppm = mean(ppm)) %>% 
  ungroup() %>% 
  mutate(is_winter = if_else(yQ %in% seasonal_months, 1, 0)) %>% 
  group_by(operator) %>% 
  ggplot(aes(x = yQ, y = ppm)) + 
  geom_rect(data = seasonal_box, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "red",
            alpha = 0.1, inherit.aes = FALSE) +
  geom_line(aes(group = operator), alpha = .7) +
  geom_vline(xintercept = as.yearqtr(seasonal_months), linetype = "dashed") + 
  scale_x_yearqtr(breaks = unique(trains$yQ)) +
  labs(y = "Passenger Performance Measure (%)",
       x = "Year - Quarter",
       title = "Figure 1: Public Performance Measure (PPM) affected by quarterly factors",
       subtitle = "Highlighted from July to January; each line is an individual operator.", 
       caption = "Data Source: ORR") +
  ylim(55, 100) + 
  theme_economist() + 
  theme(legend.position = "", 
        axis.text.x = element_text(angle = 90, margin = margin(t = 5)),
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 7)),
        title = element_text(margin = margin(b = 5)))

##############################################################################
##############################################################################

# FIGURE 2 
trains %>%
  group_by(operator, region) %>%
  arrange(yQ) %>%
  summarise(
    current_ppm = last(ppm),
    ppm_change = (last(ppm) - first(ppm)) / first(ppm) * 100) %>%
  ungroup() %>% 
  mutate(state = case_when(current_ppm > median(current_ppm) & ppm_change > 0 ~ "High & Increasing",
                           current_ppm > median(current_ppm) & ppm_change <= 0 ~ "High & Decreasing",
                           current_ppm <= median(current_ppm) & ppm_change > 0 ~ "Low & Increasing",
                           TRUE ~ "Low & Decreasing")) %>%
  ggplot(aes(x = current_ppm, y = ppm_change, color = state)) +
  geom_point(aes(shape = region), alpha = 0.7, size = 5) +
  geom_vline(xintercept = median(trains$ppm), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_text_repel(aes(label = operator), size = 3, max.overlaps = 20) +
  scale_size_continuous(range = c(2, 10), name = "Volatility %") +
  scale_color_manual(values = c("High & Increasing" = "green4", "High & Decreasing" = "orange",
                                "Low & Decreasing" = "red"), name = "Performance State", drop = FALSE) +
  labs(title = "Figure 2: Operator Performance Changes",
       subtitle = "Current Level vs Change Rate",
       x = "Current PPM Level",
       y = "PPM Change Rate (%)")

##############################################################################
##############################################################################

# FIGURE 3
trains %>% 
  group_by(yQ, operator) %>% 
  summarise(mean_ppm = mean(ppm), 
            p_km_billions = mean(p_km_billions),
            trains_planned = mean(trains_planned),
            trains_planned_per_fte = mean(trains_planned_per_fte)) %>% 
  group_by(operator) %>% 
  mutate(ppm_scale = rescale(mean_ppm, to = c(0,100)),
         p_km_billions_scaled = rescale(p_km_billions, to = c(0,100)),
         trains_planned_per_fte_scaled = rescale(trains_planned_per_fte, to = c(0,100))) %>% 
  ungroup() %>% 
  ggplot(aes(x = yQ)) + 
  geom_line(aes(y = ppm_scale, color = 'PPM')) + 
  geom_line(aes(y = p_km_billions_scaled, color = 'Passenger KM Scaled'), linetype = "dashed") + 
  geom_line(aes(y = trains_planned_per_fte_scaled, color = 'trains_planned_per_fte_scaled'), linetype = "dashed") +
  facet_wrap(~ operator) + 
  scale_color_manual(values = c("blue", "#f4a582", "darkred"), name = "") + 
  labs(title = "Figure 3: Network Usage Factors Associated with PPM",
       subtitle = "How network usage trends compare to PPM, with selected factors") + 
  theme(legend.position = "bottom")

##############################################################################
##############################################################################

# FIGURE 4
trains %>% 
  group_by(yQ, region) %>% 
  summarise(mean_ppm = mean(ppm), 
            yrly_gov_total_investment = mean(yrly_gov_total_investment),
            yrly_priv_total_investment = mean(yrly_priv_total_investment),
            total = sum(yrly_priv_total_investment, yrly_gov_total_investment)) %>% 
  ggplot(aes(x = yQ, y = mean_ppm)) + 
  geom_point(aes(size = total, colour = region), alpha = .6) + 
  geom_smooth(aes(group = region, colour = region), alpha = .15, se = TRUE) + 
  scale_size_continuous(range = c(2 , 10), guide = "none") +
  labs(title = "Figure 4: PPM and Funding over time") + 
  theme(legend.position = "bottom") + 
  scale_color_viridis_d()



