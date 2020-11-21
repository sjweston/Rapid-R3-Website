setwd("Dropbox (University of Oregon)/RAPID-EC/Data Analysis R3/Rapid-R3-Website/")
library(here)
library(stm)
library(tidyverse)
library(tidytext)
library(furrr)
library(ggpubr)
library(psych)
library(conflicted)
library(LDAvis)
library(tidystm)
library(ggrepel)
library(plotly)
library(zoo)
library(wordcloud)
conflict_prefer("summarize", "dplyr")
conflict_prefer("filter", "dplyr")

load(here("../../Data Management R3/R Data/scored_static_Nov11.Rdata"))

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
source(here("Functions/pomp.R"))
conflict_prefer("map", "purrr")
open_ended = scored %>%
  filter(Week <= 26) %>%
  filter(language != "ES") %>%
  filter(language != "SPA") %>%
  select(-contains("OPEN.006")) %>%
  filter(!is.na(race_ethnic)) %>%
  filter(!is.na(poverty_cat)) %>%
  filter(!is.na(mental_health)) %>%
  gather("question", "response", contains("OPEN")) %>%
  mutate(question = str_extract(question, ".$")) %>%
  filter(!is.na(response)) 

# word count for each response

open_ended = open_ended %>%
  mutate(word_count = map_dbl(response, ~str_count(.x, "\\S+")))

open_ended2 = open_ended %>% filter(question == "2")


processed2 <- textProcessor(open_ended2$response, metadata = open_ended2)
out2 <- prepDocuments(processed2$documents, processed2$vocab, processed2$meta)
docs2 <- out2$documents
vocab2 <- out2$vocab
meta2 <- out2$meta

heldout2 = make.heldout(docs2, vocab2)

set.seed(07312020)
plan(multiprocess)

many_models <- tibble(K =seq(5,90, by=5)) %>%
  mutate(topic_model = future_map(K, ~stm(heldout2$documents, 
                                          heldout2$vocab, 
                                          K = .,
                                          prevalence =~ race_ethnic + poverty150 + mental_health + s(Week),
                                          data = meta2, 
                                          verbose = FALSE)))
k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, heldout2$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout2$missing),
         residual = map(topic_model, checkResiduals, heldout2$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

save(many_models, k_result, file = here("data/open2_many_models.Rdata"))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  filter(K < 65) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1, alpha = 0.7, show.legend = FALSE) +
  geom_point()+
  facet_wrap(~Metric, scales = "free_y") +
  scale_x_continuous()+
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics") +
  theme_pubclean()

# looks like solution between 20 and 35 would be ideal

many_models_v2 <- tibble(K =seq(20,35, by=1)) %>%
  mutate(topic_model = future_map(K, ~stm(heldout2$documents, 
                                          heldout2$vocab, 
                                          K = .,
                                          prevalence =~ race_ethnic + poverty150 + mental_health + s(Week),
                                          data = meta2, 
                                          verbose = FALSE)))
k_result_v2 <- many_models_v2 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, heldout2$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout2$missing),
         residual = map(topic_model, checkResiduals, heldout2$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

save(many_models_v2, k_result_v2, file = here("data/open2_many_models_v2.Rdata"))

k_result_v2 %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  scale_x_continuous()+
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics") +
  theme_pubclean()

stmq2_26 <- stm(documents = docs2, 
                vocab = vocab2,
                K = 27, 
                prevalence =~ race_ethnic + poverty150 + mental_health + s(Week),
                data = meta2, 
                init.type = "Spectral")

stmq2_30 <- stm(documents = docs2, 
                vocab = vocab2,
                K = 30, 
                prevalence =~ race_ethnic + poverty150 + mental_health + s(Week),
                data = meta2, 
                init.type = "Spectral")

stmq2_34 <- stm(documents = docs2, 
                vocab = vocab2,
                K = 34, 
                prevalence =~ race_ethnic + poverty150 + mental_health + s(Week),
                data = meta2, 
                init.type = "Spectral")

save(stmq2_26, stmq2_30, stmq2_34, file = here("data/stmq2_models.Rdata"))
