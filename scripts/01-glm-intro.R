# Data and packages -------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(scales)
library(avom)
library(marginaleffects)
library(patchwork)

ess_cs <- read_rds("data/ESS10-CS.rds")
var_labs <- tibble(item = names(ess_cs),
                   label = map_chr(ess_cs, ~attr(.x, "label")))

ess_cs <- ess_cs |> 
  mutate(health = fct_na_level_to_value(health, c("Refusal", "Don't know", "No answer")),
         age = as.numeric(as.character(agea)),
         edu = fct_collapse(edlvdcz,
                            `Elementary` = levels(ess_cs$edlvdcz)[1:3],
                            `Highschool without diploma` = levels(ess_cs$edlvdcz)[4:5],
                            `Highschool with diploma` = levels(ess_cs$edlvdcz)[6:9],
                            `Universtiy` = levels(ess_cs$edlvdcz)[10:12],
                            other_level = "NA"),
         edu = fct_na_level_to_value(edu, "NA"),
         health_num = as.numeric(health))

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

# Penguins model example --------------------------------------------------
plot_penguins_linear <- penguins |> 
  mutate(sex = as.numeric(sex) - 1) |> 
  lm(sex ~ body_mass_g  * species, data = _) |> 
  plot_predictions(condition = c("body_mass_g", "species")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(labels = number_format(scale = 0.01, suffix = "kg")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values =  c("darkorange","purple","cyan4")) + 
  labs(x = "Body Mass",
       y = "P(Sex = Male)") +
  theme(legend.position = "none",
        text = element_text(size = 8))

plot_penguins_binomial <- glm(sex ~ body_mass_g  * species, data = penguins, family = binomial()) |> 
  plot_predictions(condition = c("body_mass_g", "species")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(labels = number_format(scale = 0.01, suffix = "kg")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values =  c("darkorange","purple","cyan4")) + 
  labs(x = "Body Mass",
       y = "P(Sex = Male)") +
  theme(legend.position = "none",
        text = element_text(size = 8))

ggsave(plot = plot_penguins_linear,
       filename = "plot_penguins_linear.png",
       path = "figures",
       device = ragg::agg_png,
       dpi = 300,
       units = "cm",
       width = 8,
       height = 8)

ggsave(plot = plot_penguins_linear,
       filename = "plot_penguins_linear_small.png",
       path = "figures",
       device = ragg::agg_png,
       dpi = 300,
       units = "cm",
       width = 8,
       height = 5,
       scaling = 0.9)

ggsave(plot = plot_penguins_binomial,
       filename = "plot_penguins_binomial_small.png",
       path = "figures",
       device = ragg::agg_png,
       dpi = 300,
       units = "cm",
       width = 8,
       height = 5,
       scaling = 0.9)

# Wellbeing model example -------------------------------------------------
model_health_linear <- lm(health_num ~ age * edu, data = ess_cs)

plot_health_linear <- model_health_linear |> 
  plot_predictions(condition = c("age", "edu")) +
  labs(x = "Age",
       y = "General Health") +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  scale_y_reverse(breaks = 1:5,
                     labels = levels(ess_cs$health)[1:5]) +
  theme(legend.position = c(0.2,0.2),
        legend.title = element_blank(),
        legend.text = element_text(size = 4),
        text = element_text(size = 8),
        legend.margin = margin(),
        legend.key.height = unit(0.3, "cm"))

plot_health_linear_residuals <- tibble(residual = residuals(model_health_linear),
                                       fitted = predict(model_health_linear)) |> 
  ggplot(aes(x = fitted,
             y = residual)) +
  geom_point(alpha = 0.5, size = 0.3) +
  geom_smooth(linewidth = 0.5) +
  labs(title = "Residuals vs fitted") +
  theme(text = element_text(size = 5))

ggsave(plot = plot_health_linear,
       filename = "plot_health_linear.png",
       path = "figures",
       device = ragg::agg_png,
       dpi = 300,
       units = "cm",
       width = 8,
       height = 8)

ggsave(plot = plot_health_linear_residuals,
       filename = "plot_health_linear_residuals.png",
       path = "figures",
       device = ragg::agg_png,
       dpi = 300,
       units = "cm",
       width = 6,
       height = 6)

# Wellbeing binomial model ------------------------------------------------
model_health_binomial <- ess_cs |> 
  mutate(success = health_num,
         fail = 5 - health_num) |> 
  glm(cbind(success, fail) ~ age * edu,
      data = _,
      family = binomial())

dharma_res <- DHARMa::simulateResiduals(model_health_binomial, plot = FALSE)
plot_health_binomial_residuals <- tibble(residual = dharma_res$scaledResiduals,
                                      fitted = predict(model_health_binomial)) |> 
  ggplot(aes(x = fitted,
             y = residual)) +
  geom_point(alpha = 0.3, size = 0.3) +
  geom_smooth(linewidth = 0.5) +
  labs(title = "Residuals vs fitted") +
  theme(text = element_text(size = 5))

ggsave(plot = plot_health_binomial_residuals,
       filename = "plot_health_binomial_residuals.png",
       path = "figures",
       device = ragg::agg_png,
       dpi = 300,
       units = "cm",
       width = 6,
       height = 6)

# Distribution examples ---------------------------------------------------

plot_dist_binomial_example <- tibble(x = c("Voters", "Non-voters"),
       y = c(0.6, 0.4)) |> 
  ggplot(aes(x = x,
             y = y)) +
  geom_col() +
  labs(x = element_blank(),
       y = element_blank())

plot_dist_poisson_example <- tibble(x = 0:10,
       y = dpois(x, lambda = 2)) |> 
  ggplot(aes(x = x,
             y = y)) +
  geom_col() +
  labs(x = element_blank(),
       y = element_blank()) +
  scale_x_continuous(breaks = c(0:10))

plot_dist_beta_example <- tibble(x = seq(0,1,0.01 ),
       y = dbeta(x, shape1 = 5, shape2 = 2)) |> 
  ggplot(aes(x = x,
             y = y)) +
  geom_area() +
  labs(x = element_blank(),
       y = element_blank()) +
  scale_x_continuous(labels = percent_format(accuracy = 1))
  
plots_dist_examples <- mget(ls(pattern = "plot_dist"))
plots_dist_names <- paste0(names(plots_dist_examples), ".png")

walk2(.x = plots_dist_examples,
      .y = plots_dist_names,
      ~ggsave(plot = .x,
              filename = .y,
              path = "figures",
              device = ragg::agg_png,
              units = "cm",
              width = 6,
              height = 3,
              scaling = 0.8))
