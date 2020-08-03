setwd("/Users/sweston2/Dropbox (University of Oregon)/Rapid Response Research (R3)/Data Analysis R3/R code Rapid-R3-Website")
library(here)
library(stm)
library(tidyverse)
library(tidytext)
library(furrr)
load("../../Data Management R3/R Data/scored.Rdata")

open_ended = scored %>%
  select(CaregiverID, Week, contains("OPEN")) %>%
  select(-contains("006")) %>%
  gather("question", "response", contains("OPEN")) %>%
  mutate(question = str_extract(question, ".$")) %>%
  filter(!is.na(response)) %>%
  filter(!(response %in% c("", "None","None.","NA","NA.",
                           "none","none.","na","na.","Na", "Na.", 
                           "N/A","N/A.", "N/a","N/a.", "n/a","n/a.",
                           "No", "No.", "Nope", "nope","Nope.", "nope.", "no", "no.", "No thank you", "No thank you.",
                           "no thank you", "no thank you.","No, thank you", "No, thank you.",
                           "no, thank you", "no, thank you.", "Nothing", "ty", "nothing", "not really", "Not really", "Ni", "ni", 
                           " No", "Not at this time", "No, thx", "Nope !", "NONE", "Nope!", "not at this time", 
                           "Not at the moment", "No thanks", "no thanks", "Nothing I can think of.", "Nothing I can think of.",
                           "^no[.]{0-6}", "^No[.]{0-6}", "Not right now", "not right now", "I do not"))) %>%
  group_by(question) %>%
  nest()

open1_tidy = open_ended$data[[1]] %>%
  unnest_tokens(output = word, input = response, token = "words") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) 

processed1 <- textProcessor(open_ended$data[[1]]$response, metadata = open_ended$data[[1]])
out1 <- prepDocuments(processed1$documents, processed1$vocab, processed1$meta)
docs1 <- out1$documents
vocab1 <- out1$vocab
meta1 <- out1$meta

heldout1 = make.heldout(docs1, vocab1)

set.seed(07312020)
plan(multiprocess)

many_models1 <- tibble(K = c(2:30)) %>%
  mutate(topic_model = future_map(K, ~stm(heldout1$documents, heldout1$vocab, K = .,
                                          verbose = FALSE)))
k_result1 <- many_models1 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, heldout1$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout1$missing),
         residual = map(topic_model, checkResiduals, heldout1$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

save(many_models1, file = here("../../Data Management R3/R Data/open1_many_models.Rdata"))

rm(many_models1)

open2_tidy = open_ended$data[[2]] %>%
  unnest_tokens(output = word, input = response, token = "words") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) 

processed2 <- textProcessor(open_ended$data[[2]]$response, metadata = open_ended$data[[2]])
out2 <- prepDocuments(processed2$documents, processed2$vocab, processed2$meta)
docs2 <- out2$documents
vocab2 <- out2$vocab
meta2 <- out2$meta

heldout2 = make.heldout(docs2, vocab2)

set.seed(07322020)
plan(multiprocess)

many_models2 <- tibble(K = c(2:30)) %>%
  mutate(topic_model = future_map(K, ~stm(heldout2$documents, heldout2$vocab, K = .,
                                          verbose = FALSE)))

k_result2 <- many_models2 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, heldout2$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout2$missing),
         residual = map(topic_model, checkResiduals, heldout2$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

save(many_models2, k_result2, file = here("../../Data Management R3/R Data/open2_many_models.Rdata"))

rm(many_models2)

open3_tidy = open_ended$data[[3]] %>%
  unnest_tokens(output = word, input = response, token = "words") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) 

processed3 <- textProcessor(open_ended$data[[3]]$response, metadata = open_ended$data[[3]])
out3 <- prepDocuments(processed3$documents, processed3$vocab, processed3$meta)
docs3 <- out3$documents
vocab3 <- out3$vocab
meta3 <- out3$meta

heldout3 = make.heldout(docs3, vocab3)

set.seed(07332020)
plan(multiprocess)

many_models3 <- tibble(K = c(2:30)) %>%
  mutate(topic_model = future_map(K, ~stm(heldout3$documents, heldout3$vocab, K = .,
                                          verbose = FALSE)))

k_result3 <- many_models3 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, heldout3$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout3$missing),
         residual = map(topic_model, checkResiduals, heldout3$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

save(many_models3, k_result3, file = here("../../Data Management R3/R Data/open3_many_models.Rdata"))

rm(many_models3)

open4_tidy = open_ended$data[[4]] %>%
  unnest_tokens(output = word, input = response, token = "words") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) 

processed4 <- textProcessor(open_ended$data[[4]]$response, metadata = open_ended$data[[4]])
out4 <- prepDocuments(processed4$documents, processed4$vocab, processed4$meta)
docs4 <- out4$documents
vocab4 <- out4$vocab
meta4 <- out4$meta

heldout4 = make.heldout(docs4, vocab4)

set.seed(07342020)
plan(multiprocess)

many_models4 <- tibble(K = c(2:30)) %>%
  mutate(topic_model = future_map(K, ~stm(heldout4$documents, heldout4$vocab, K = .,
                                          verbose = FALSE)))

k_result4 <- many_models4 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, heldout4$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout4$missing),
         residual = map(topic_model, checkResiduals, heldout4$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

save(many_models4, k_result4, file = here("../../Data Management R3/R Data/open4_many_models.Rdata"))

rm(many_models4)

open5_tidy = open_ended$data[[5]] %>%
  unnest_tokens(output = word, input = response, token = "words") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) 

processed5 <- textProcessor(open_ended$data[[5]]$response, metadata = open_ended$data[[5]])
out5 <- prepDocuments(processed5$documents, processed5$vocab, processed5$meta)
docs5 <- out5$documents
vocab5 <- out5$vocab
meta5 <- out5$meta

heldout5 = make.heldout(docs5, vocab5)

set.seed(07352020)
plan(multiprocess)

many_models5 <- tibble(K = c(2:30)) %>%
  mutate(topic_model = future_map(K, ~stm(heldout5$documents, heldout5$vocab, K = .,
                                          verbose = FALSE)))
k_result5 <- many_models5 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, heldout5$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout5$missing),
         residual = map(topic_model, checkResiduals, heldout5$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

save(many_models5, k_result5, file = here("../../Data Management R3/R Data/open5_many_models.Rdata"))

rm(many_models5)
