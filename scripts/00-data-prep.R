library(tidyverse)
library(labelled)
library(haven)

# Europeans Social Survey 2022 --------------------------------------------
ess_full <- read_rds("data/ESS10-CS.rds")
ess_full <- droplevels(ess_full)
edu_levels <- levels(ess_full$edlvdcz)

ess <- ess_full |> 
  select(age = agea,
         gender = gndr,
         education = edlvdcz,
         health,
         income_inequalities = gincdif,
         vote,
         party = prtvtecz,
         internet_use = netustm) |> 
  droplevels() |> 
  mutate(age = as.numeric(as.character(age)),
         vote = fct_na_level_to_value(vote, "Not eligible to vote"),
         education = fct_collapse(education,
                                "Elementary" = edu_levels[1:2],
                                "Highschool (without diploma)" = edu_levels[3:4],
                                "Highschool (with diploma)" = edu_levels[5:8],
                                "University" = edu_levels[9:11]),
         internet_use = as.numeric(as.character(internet_use)),
         ppl)

var_label(ess) <- list("age" = "Age",
                       "gender" = "Gender",
                       "education" = "Education",
                       "health" = "General subjective health",
                       "income_inequalities" = "Government should reduce differences in income levels",
                       "vote" = "Voted in the last parliamentary election",
                       "party" = "Party voted for in the last parliamentary election",
                       "internet_use" = "Internet use in minutes per day")

write_rds(ess, "data/ess.rds")

# School absences ---------------------------------------------------------
absences <- haven::read_dta("https://stats.idre.ucla.edu/stat/stata/dae/nb_data.dta")
absences <- haven::as_factor(absences)
absences$prog <- factor(absences$prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))

write_rds(absences, "data/absences.rds")

# CERV --------------------------------------------------------------------
cerv <- read_rds("data/cerv.rds")
cerv <- as_factor(cerv)

cerv <- cerv |> 
  filter(country == "Czech Republic") |> 
  select(starts_with("t0"), eu_index_cat, age, edu, eu_index, eu_response_ua = a05czskd) |> 
  rename_with(.cols = starts_with("t0"), .fn = ~str_replace(., "t0", "eu_test")) |> 
  mutate(eu_test1 = eu_test1 == "European Parliament",
         eu_test2 = eu_test2 == "Ursula von der Leyen",
         eu_test3 = eu_test3 == "Norway and Switzerland",
         eu_test4 = eu_test4 == "Customs policy (amount of import duties)",
         eu_index_cat = case_when(eu_index_cat == "1 - 1.75" ~ "Strongly pro-EU",
                                  eu_index_cat == "1.75 - 2.5" ~ "Pro-EU",
                                  eu_index_cat == "2.5 - 3.25" ~ "Eurosceptic",
                                  eu_index_cat == "3.25 - 4" ~ "Strongly eurosceptic"),
         eu_index_cat = fct_relevel(eu_index_cat,
                                    "Strongly pro-EU",
                                    "Pro-EU",
                                    "Eurosceptic",
                                    "Strongly eurosceptic"),
         age = as.numeric(as.character(age))) |> 
  rowwise() |> 
  mutate(eu_knowledge = sum(c_across(starts_with("eu_test")))) |> 
  ungroup() |> 
  relocate(age, edu, eu_response_ua)

var_label(cerv) <- list("eu_test1" = "Members of which of the following institutions are elected by the people directly?" ,
                        "eu_test2" = "Who is currently the President of the European Commission?",
                        "eu_test3" = "Which two countries are not EU members?",
                        "eu_test4" = "In which of the following areas does Czechia have the least freedom in setting its own policy as a result of its membership in the EU?")

write_rds(cerv, "data/eu-attitudes.rds")

# Buzzwords ---------------------------------------------------------------
buzzwords <- read_sav("data/buzzwords.sav")
buzzwords <- as_factor(buzzwords)

buzzwords <- buzzwords |> select(pom_heslo, starts_with("heslo"))
write_rds(buzzwords, "data/buzzwords.rds")
