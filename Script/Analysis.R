################################################################################
#
# Script name:          Analysis.R
# Script description:   This script analyses the effect of limiting longterm
#                       illness (LLTI) on feeling optimism about the future 
#                       using Health Survey for England (2019) data.
#
# Author:               @KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse)
library(dagitty)
library(ggdag)
library(broom)

# Run data formatting script
source("./Script/Data_processing.R")

# ANALYSIS ---------------------------------------------------------------------

# Download published DAG from dagitty.net
dag <- downloadGraph(x = "dagitty.net/m3DXjao")

# Create 'tidy' DAG with node status (i.e. exposure/outcome) & variable labels
tidy_dag <- dag %>%
  tidy_dagitty %>%
  node_status(as_factor = FALSE) %>%
  mutate(label = str_replace_all(string = name, 
                                 pattern = "_", 
                                 replacement = "\n")) 

# Plot DAG
plot_dag <- tidy_dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) + 
  geom_dag_label_repel(aes(label = label)) +
  labs(title = "Directed acyclic graph (DAG)",
       subtitle = "for the effect of limiting longterm illness (LLTI) on feeling optimistim about the future") +
  theme_dag()
plot_dag

# Calculate minimally sufficient adjustment set (MSAS) & denote variables in tidy DAG
msas <- adjustmentSets(x = dag, type = "minimal", effect = "total") %>% pluck(1)
tidy_dag <- tidy_dag %>%
  mutate(status = ifelse(name %in% msas, "msas", status))

# Plot MSAS
plot_msas <- tidy_dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) + 
  geom_dag_label_repel(aes(label = label)) +
  labs(title = "Directed acyclic graph (DAG) with minimally sufficient adjustment set (MSAS) identified" ,
       subtitle = "for the effect of limiting longterm illness (LLTI) on feeling optimistim about the future") +
  theme_dag()
plot_msas

# Fit GLM for outcome ~ exposure, adjusted for MSAS & Survey_qtr
mod <- glm(formula = Optimism ~ LLTI + Age + Sex + Ethnicity + Education + BMI + Survey_qtr,
           family = "quasibinomial",
           weights = Sample_weight,
           data = data)

# View tidy summary of fitted model
tidy(mod, conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  filter(str_detect(term, "LLTI"))

# EXPORT OUTPUT ----------------------------------------------------------------

# Export DAGs as .png files
ggsave(filename = "DAG.jpg",
       plot = plot_dag,
       path = "./Output/")
ggsave(filename = "DAG_with_MSAS.jpg",
       plot = plot_msas,
       path = "./Output/")

