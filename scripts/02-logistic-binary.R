library(tidyverse)
library(palmerpenguins)
library(marginaleffects)
library(avom)
library(gganimate)
library(performance)
library(DHARMa)
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

# Binomial distribution examples ------------------------------------------
plot_bin_binomial_example1 <- tibble(x = 0:10,
       y = dbinom(x, size = 10 ,prob = 0.5)) |> 
  ggplot(aes(x = x,
             y = y)) + 
  geom_col() +
  scale_x_continuous(breaks = 0:10) +
  labs(x = element_blank(),
       y = element_blank())

plot_bin_binomial_example2 <- tibble(x = 0:5,
       y = dbinom(x, size = 5 ,prob = 0.8)) |> 
  ggplot(aes(x = x,
             y = y)) + 
  geom_col() +
  scale_x_continuous(breaks = 0:5) +
  labs(x = element_blank(),
       y = element_blank())

plot_bin_binomial_example3 <- tibble(x = 0:1,
       y = dbinom(x, size = 1 ,prob = 0.2)) |> 
  ggplot(aes(x = x,
             y = y)) + 
  geom_col() +
  scale_x_continuous(breaks = 0:1) +
  labs(x = element_blank(),
       y = element_blank())

binomial_examples <- mget(ls(pattern = "plot_bin_binomial_example"))

walk2(.x = binomial_examples,
      .y = names(binomial_examples),
      .f = ~ggsave(plot = .x,
                   filename = paste0(.y,".png"),
                   path = "figures",
                   device = ragg::agg_png,
                   units = "cm",
                   width = 6,
                   height = 2,
                   scaling = 0.5,
                   dpi = 300))

# Link function examples --------------------------------------------------
plot_lg_link_identity_example <- tibble(x = seq(0,1, 0.0001),
       y = x) |> 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(x = "Transformed variable",
       y = "Original Variable")

plot_lg_link_log_example <- tibble(x = seq(0,1, 0.0001),
                                   y = log(x)) |> 
  ggplot(aes(x = y,
             y = x)) +
  geom_line() +
  labs(x = "Transformed variable",
       y = "Original Variable")

plot_lg_link_logit_example <- tibble(x = seq(0,1, 0.0001),
                                y = log(x / (1 - x))) |> 
  ggplot(aes(x = y,
             y = x)) +
  geom_line() +
  labs(x = "Transformed variable",
       y = "Original Variable")

link_examples <- mget(ls(pattern = "plot_lg_link"))

walk2(.x = link_examples,
      .y = names(link_examples),
      .f = ~ggsave(plot = .x,
                   filename = paste0(.y,".png"),
                   path = "figures",
                   device = ragg::agg_png,
                   units = "cm",
                   width = 8,
                   height = 6,
                   scaling = 0.8,
                   dpi = 300))

# Noncollapsibility -------------------------------------------------------

## Linear regresion
set.seed(1234)
age <- rbinom(n = 1e6, prob = 0.4, size = 62) + 18
male <- rbinom(n = 1e6, prob = 0.5, size = 1)
income_thousands <- rnorm(n = 1e6, mean = 38, sd = 5)
wellbeing <- rnorm(n = 1e6, mean = -150 + 1*age + 2*male +3*income_thousands, sd = 15)

m1_lin <- lm(wellbeing ~ age)
m2_lin <- lm(wellbeing ~ age + male)
m3_lin <- lm(wellbeing ~ age + male + income_thousands)


plot_collapse_lin_marg <- m1_lin |> 
  plot_predictions(condition = "age", draw = FALSE) |> 
  ggplot(aes(x = age,
             y = estimate,
             color = "Marginal")) +
  geom_line() +
  scale_color_manual(values = "#83a598") +
  labs(x = "Age",
       y = "Wellbeing",
       color = element_blank()) +
  theme(legend.position = c(0.75,0.2))

plot_collapse_lin_group <- m2_lin |> 
  plot_predictions(condition = c("age", "male"), draw = FALSE) |> 
  mutate(male = if_else(male == 1, "Men", "Women"),
         male = fct_rev(male)) |> 
  ggplot(aes(x = age,
             y = estimate,
             color = male)) +
  geom_line() +
  labs(x = "Age",
       y = "Wellbeing",
       color = element_blank()) +
  theme(legend.position = c(0.75,0.2)) +
  guides(color = guide_legend(reverse = TRUE))

ggsave(plot = plot_collapse_lin_group,
       filename = "collapsibility_linear_group.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)

ggsave(plot = plot_collapse_lin_marg,
         filename = "collapsibility_linear_marginal.png",
         path = "figures",
         device = ragg::agg_png,
         units = "cm",
         width = 8,
         height = 6,
         scaling = 0.8,
         dpi = 300)

## Logistic regression
set.seed(1234)
y_lin <- -80 + 1*age + 2*male + 1*income_thousands
y_prob <- 1 / (1 + exp(-y_lin))
turnout <- rbinom(n = 1e6, size = 1, prob = y_prob)

m1_binom <- glm(turnout ~ age, family = binomial())
m2_binom <- glm(turnout ~ age + male, family = binomial())
m3_binom <- glm(turnout ~ age + male + income_thousands, family = binomial())

## Comparing countries example
y_lin_usa <- -180 + 1*age + 2*male + 4*income_thousands
y_prob_usa <- 1 / (1 + exp(-y_lin_usa))
turnout_usa <- rbinom(n = 1e6, size = 1, prob = y_prob_usa)

m1_binom_usa <- glm(turnout_usa ~ age, family = binomial())

## Graphical examples
m1_linpred <- plot_predictions(m1_lin, condition = "age", draw = FALSE)
m2_linpred <- plot_predictions(m2_lin, condition = c("age", "male"), draw = FALSE)

m1_binpred <- plot_predictions(m1_binom, condition = "age", draw = FALSE, type = "link")
m2_binpred <- plot_predictions(m3_binom, condition = c("age", "male"), draw = FALSE, type = "link")

## Linear regression animation
linpred_state1 <- bind_rows(m1_linpred,
          m2_linpred,
          .id = "model") |> 
  mutate(male = case_when(is.na(male) ~ "Marginal",
                          male == 1 ~ "Men",
                          male == 0 ~ "Women"),
         state = 1)

linpred_state2 <- linpred_state1 |>
  mutate(estimate = case_when(male == "Men" ~ estimate - 1,
                              male == "Women" ~ estimate + 1,
                              TRUE ~ estimate),
         state = 2)

anim_collapsibility_lin <- bind_rows(linpred_state1,
          linpred_state2) |> 
  mutate(male = fct_relevel(male,
                            "Men",
                            "Marginal",
                            "Women")) |> 
  ggplot(aes(x = age,
             y = estimate,
             color = male)) +
  geom_line() +
  scale_color_manual(values = c("#00BFC4", "#83a598", "#F8766D")) +
  transition_states(states = state,
                    transition_length = 3,
                    state_length = 1) +
  labs(x = "Age",
       y = "Wellbeing",
       color = element_blank()) +
  theme(legend.position = c(0.75,0.2),
        text = element_text(size = 7),
        legend.text = element_text(margin = margin(t = 0.5, unit = "pt"))) +
  guides(color = guide_legend(reverse = TRUE))

anim_save(animation = anim_collapsibility_lin,
          filename = "anim_collapsibility_lin.gif",
          path = "figures",
          units = "cm",
          width = 8,
          height = 6,
          res = 300)

## Logistic regression animation
binpred_state1 <- bind_rows(m1_binpred,
                            m2_binpred,
                            .id = "model") |> 
  mutate(male = case_when(is.na(male) ~ "Marginal",
                          male == 1 ~ "Men",
                          male == 0 ~ "Women"),
         state = 1)

binpred_state2 <- binpred_state1 |>
  mutate(estimate = case_when(male == "Men" ~ estimate - 1,
                              male == "Women" ~ estimate + 1,
                              TRUE ~ estimate),
         state = 2)


anim_collapsibility_binom <- bind_rows(binpred_state1,
                                     binpred_state2) |> 
  mutate(male = fct_relevel(male,
                            "Men",
                            "Marginal",
                            "Women")) |> 
  ggplot(aes(x = age,
             y = estimate,
             color = male)) +
  geom_line() +
  scale_color_manual(values = c("#00BFC4", "#83a598", "#F8766D")) +
  transition_states(states = state,
                    transition_length = 3,
                    state_length = 1) +
  labs(x = "Age",
       y = "Logit(Turnout)",
       color = element_blank()) +
  theme(legend.position = c(0.75,0.2),
        text = element_text(size = 7),
        legend.text = element_text(margin = margin(t = 0.5, unit = "pt"))) +
  guides(color = guide_legend(reverse = TRUE))

anim_save(animation = anim_collapsibility_binom,
          filename = "anim_collapsibility_binom.gif",
          path = "figures",
          units = "cm",
          width = 8,
          height = 6,
          res = 300)

# Latent variables --------------------------------------------------------
plot_dlogis1 <- tibble(x = seq(-10,10, 0.1),
       value = dlogis(x = x, scale = 1.81)) |> 
  ggplot(aes(x = x,
             y = value)) +
  geom_line() +
  labs(x = "Propensity to being male",
       y = element_blank())

plot_dlogis2 <- tibble(x = seq(-15,15, 0.1),
                       value = dlogis(x = x, scale = 1.81, location = -5)) |> 
  ggplot(aes(x = x,
             y = value)) +
  geom_line() +
  labs(x = "Propensity to being male",
       y = element_blank())

plot_dlogis3 <- tibble(x = seq(-15,15, 0.1),
                       value = dlogis(x = x, scale = 1, location = 3)) |> 
  ggplot(aes(x = x,
             y = value)) +
  geom_line() +
  labs(x = "Propensity to being male",
       y = element_blank())


dlogis_examples <- mget(ls(pattern = "plot_dlogis"))

walk2(.x = dlogis_examples,
      .y = names(dlogis_examples),
      .f = ~ggsave(plot = .x,
                   filename = paste0(.y,".png"),
                   path = "figures",
                   device = ragg::agg_png,
                   units = "cm",
                   width = 8,
                   height = 6,
                   scaling = 0.8,
                   dpi = 300))

walk2(.x = dlogis_examples,
      .y = names(dlogis_examples),
      .f = ~ggsave(plot = .x,
                   filename = paste0(.y,"_small.png"),
                   path = "figures",
                   device = ragg::agg_png,
                   units = "cm",
                   width = 8,
                   height = 3,
                   scaling = 0.6,
                   dpi = 300))

# Marginal effects --------------------------------------------------------
plot_marginal_example <- plot_predictions(m1_binom, condition = "age", draw = FALSE) |> 
  ggplot(aes(x = age,
             y = estimate)) +
  geom_line() +
  geom_segment(mapping = aes(x = 40,
                             xend = 40,
                             y = 0.422,
                             yend = 0),
               linetype = "dashed") +
  geom_abline(slope = 0.0717,
              intercept = -2.44,
              color = "#D65D0E") +
  annotate(geom = "point",
           x = 40,
           y = 0.422,
           color = "#D65D0E",
           size = 3) +
  annotate(geom = "text",
           x = 50,
           y = 0.43,
           label = "Slope = 0.0717",
           color = "#D65D0E")

ggsave(plot = plot_marginal_example,
       filename = "plot_marginal_example.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)



slopes(m1_binom,
       newdata = tibble(age = c(40:45)))

age_predictions <- predictions(m1_binom,
       newdata = tibble(age = c(40:45))) |> 
  pull(estimate)

age_eps_predictions <- predictions(m1_binom,
            newdata = tibble(age = c(40:45) + 0.001))|> 
  pull(estimate)

(age_eps_predictions - age_predictions) * 100


plot_predictions(m1_binom, condition = "age", draw = FALSE) |> 
  ggplot(aes(x = age,
             y = estimate)) +
  geom_line()


plot_marginal_average_example <- slopes(m1_binom, condition = "age", newdata = tibble(age = seq(30, 60, 2))) |> 
  ggplot(aes(x = age,
             y = predicted)) +
  geom_line() +
  geom_point(color = "#D65D0E") +
  geom_text(aes(x = age + 3,
                y = predicted,
                label = round(estimate, 3)),
            color = '#D65D0E',
            size = 3)


ggsave(plot = plot_marginal_average_example,
       filename = "plot_marginal_average_example.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)

# Linear Probability Models -----------------------------------------------
m1_lpm <- lm(turnout ~ age)
m2_lpm <- lm(turnout ~ age + male)
m3_lpm <- lm(turnout ~ age + male + income_thousands)

# Binned residuals --------------------------------------------------------
penguins_binom <- glm(sex ~ body_mass_g, data = penguins, family = binomial())
penguins_binom2 <- glm(sex ~ body_mass_g + species, data = penguins, family = binomial())

plot_penguin_resid_basic <- tibble(residual = residuals(penguins_binom),
       predicted = predict(penguins_binom)) |> 
  ggplot(aes(x = predicted,
             y = residual)) +
  geom_point()

ggsave(plot = plot_penguin_resid_basic,
       filename = "plot_penguin_resid_basic.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.8,
       dpi = 300)

plot_penguin_resid_binned <- penguins_binom |> 
  binned_residuals() |> 
  plot() +
  theme(legend.position = "bottom")

plot_penguin_resid_binned_correct <- penguins_binom2 |> 
  binned_residuals() |> 
  plot() +
  theme(legend.position = "bottom")

ggsave(plot = plot_penguin_resid_binned,
       filename = "plot_penguin_resid_binned.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.6,
       dpi = 300)

ggsave(plot = plot_penguin_resid_binned_correct,
       filename = "plot_penguin_resid_binned_correct.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.6,
       dpi = 300)

# Randomized residuals ----------------------------------------------------
plotResiduals(penguins_binom)
DHARMa::plotResiduals(penguins_binom, quantiles = 0.5)
DHARMa::plotResiduals(penguins_binom2, quantiles = 0.5)

plot_penguin_resid_ranodmised <- tibble(predicted = rank(predict(penguins_binom)),
       residuals = DHARMa::simulateResiduals(penguins_binom)$scaledResiduals) |> 
  ggplot(aes(x = predicted,
             y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0.5,
             color = "#D65D0E") +
  geom_smooth() +
  labs(x = "Predicted values (ranks)",
       y = "Randomised residuals")

plot_penguin_resid_ranodmised_correct <- tibble(predicted = rank(predict(penguins_binom2)),
                                        residuals = DHARMa::simulateResiduals(penguins_binom2)$scaledResiduals) |> 
  ggplot(aes(x = predicted,
             y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0.5,
             color = "#D65D0E") +
  geom_smooth() +
  labs(x = "Predicted values (ranks)",
       y = "Randomised residuals")

ggsave(plot = plot_penguin_resid_ranodmised,
       filename = "plot_penguin_resid_ranodmised.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.6,
       dpi = 300)

ggsave(plot = plot_penguin_resid_ranodmised_correct,
       filename = "plot_penguin_resid_ranodmised_correct.png",
       path = "figures",
       device = ragg::agg_png,
       units = "cm",
       width = 8,
       height = 6,
       scaling = 0.6,
       dpi = 300)
