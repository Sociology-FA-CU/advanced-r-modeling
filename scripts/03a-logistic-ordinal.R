library(ordinal)
library(tidyverse)
library(marginaleffects)
library(avom)
library(performance)
library(rms)

ess <- read_rds("data/ess.rds")

# Plot setup --------------------------------------------------------------
theme_set(theme_avom() +
            theme(text = element_text(size = 10),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.background = element_rect(fill = "#FFF0C1"),
                  plot.background = element_rect(fill = "#FFF0C1"),
                  axis.ticks = element_blank()))

update_geom_defaults("col", list(fill = "#83a598"))
update_geom_defaults("area", list(fill = "#83a598"))
update_geom_defaults("line", list(colour = "#83a598"))

# General health plot -----------------------------------------------------
plot_ordinal_health <- ess |> 
  filter(!is.na(health)) |>
  count(health) |> 
  mutate(prop = n / sum(n)) |> 
  ggplot(aes(x = health,
             y = prop,
             fill = health)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = rev(c("#b9211a", "#fb4934", "#fac64c", "#83a598", "#458588"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = element_blank(),
       y = element_blank(),
       title = "General Subjective Health")

ggsave(plot = plot_ordinal_health,
       filename = "plot_ordinal_health.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)

# Ordinal latent plot -----------------------------------------------------
ord_latent <- tibble(x = seq(-5,5, 0.1), y = dlogis(x))

plot_ordinal_latent <- ggplot(data = ord_latent,
       aes(x = x,
           y = y)) +
  geom_area(data = filter(ord_latent, x <= -3),
            mapping = aes(fill = "Very good")) + 
  geom_area(data = filter(ord_latent, x <= -1 & x >= -3),
            mapping = aes(fill = "Good")) + 
  geom_area(data = filter(ord_latent, x <= 1 & x >= -1),
            mapping = aes(fill = "Fair")) + 
  geom_area(data = filter(ord_latent, x <= 3 & x >= 1),
            mapping = aes(fill = "Bad")) +
  geom_area(data = filter(ord_latent, x >= 3),
            mapping = aes(fill = "Very bad")) +
  scale_fill_manual(values = rev(c("#b9211a", "#fb4934", "#fac64c", "#83a598", "#458588")),
                    breaks = rev(c("Very bad", "Bad", "Fair", "Good", "Very good"))) +
  labs(x = element_blank(),
       y = element_blank(),
       fill = "Subjective General Health") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

ggsave(plot = plot_ordinal_latent,
       filename = "plot_ordinal_latent.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)

# Ordinal regression ------------------------------------------------------
m1 <- clm(health ~ education + age, data = ess)

plot_health_age <- plot_predictions(m1, condition = c("age", "group"),
                 draw = FALSE) |> 
  mutate(group = fct_relevel(group, levels(ess$health))) |> 
  ggplot(aes(x = age,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = group,
             fill = group)) +
  geom_ribbon(alpha = 0.4, color = NA) +
  geom_line() +
  scale_fill_manual(values = rev(c("#b9211a", "#fb4934", "#fac64c", "#83a598", "#458588"))) +
  scale_color_manual(values = rev(c("#b9211a", "#fb4934", "#fac64c", "#83a598", "#458588"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = element_blank(),
       y = element_blank(),
       color = "Subjective General Health",
       fill = "Subjective General Health") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

ggsave(plot = plot_health_age,
       filename = "plot_ordinal_health_age.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)

plot_health_edu <- plot_predictions(m1, condition = c("education", "group"),
                 draw = FALSE) |> 
  mutate(group = fct_relevel(group, levels(ess$health))) |> 
  ggplot(aes(x = education,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = group,
             group = group)) +
  geom_pointrange() +
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = rev(c("#b9211a", "#fb4934", "#fac64c", "#83a598", "#458588"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = ~str_wrap(., width = 15)) +
  labs(x = element_blank(),
       y = element_blank(),
       color = "Subjective General Health") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                             title.hjust = 0.5))

ggsave(plot = plot_health_edu,
       filename = "plot_ordinal_health_edu.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)

# Assumption checking -----------------------------------------------------
m2 <- clm(health ~ education + age, nominal = ~education, data = ess)
m3 <- clm(health ~ education + age, nominal = ~age, data = ess)

plot_ordinal_health_age_nominal <- plot_predictions(m3, condition = c("age", "group"),
                 draw = FALSE) |> 
  mutate(group = fct_relevel(group, levels(ess$health))) |> 
  ggplot(aes(x = age,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = group,
             fill = group)) +
  geom_ribbon(alpha = 0.4, color = NA) +
  geom_line() +
  scale_fill_manual(values = rev(c("#b9211a", "#fb4934", "#fac64c", "#83a598", "#458588"))) +
  scale_color_manual(values = rev(c("#b9211a", "#fb4934", "#fac64c", "#83a598", "#458588"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = element_blank(),
       y = element_blank(),
       color = "Subjective General Health",
       fill = "Subjective General Health") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

plot_ordinal_health_edu_nominal <- plot_predictions(m2, condition = c("education", "group"),
                                    draw = FALSE) |> 
  mutate(group = fct_relevel(group, levels(ess$health))) |> 
  ggplot(aes(x = education,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = group,
             group = group)) +
  geom_pointrange() +
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = rev(c("#b9211a", "#fb4934", "#fac64c", "#83a598", "#458588"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = ~str_wrap(., width = 15)) +
  labs(x = element_blank(),
       y = element_blank(),
       color = "Subjective General Health") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5))


ggsave(plot = plot_ordinal_health_age_nominal,
       filename = "plot_ordinal_health_age_nominal.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)

ggsave(plot = plot_ordinal_health_edu_nominal,
       filename = "plot_ordinal_health_edu_nominal.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)
