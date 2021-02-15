
# packages ----------------------------------------------------------------

library(here)
library(stm)
library(tidyverse)
library(tidytext)
library(furrr)


# data --------------------------------------------------------------------

load("../../Data Management R3/R Data/scored.Rdata")


# cleaning ----------------------------------------------------------------

open_ended = scored %>%
  filter(language != "ES") %>% filter(language != "SPA") %>% # filter out non-english speaking words
  select(CaregiverID, Week, contains("OPEN"), 
         black, poverty150) %>% # these are groups that we want topics to help us distinguish
  filter(!is.na(black) & !is.na(poverty150)) %>%
  select(-contains("006")) %>% # get rid of "anythin gelse?" question
  gather("question", "response", contains("OPEN")) %>%
  mutate(question = str_extract(question, "..$")) %>%
  filter(!is.na(response)) %>%
  filter(!(response %in% c("", "None","None.","NA","NA.",
                           "none","none.","na","na.","Na", "Na.", 
                           "N/A","N/A.", "N/a","N/a.", "n/a","n/a.",
                           "No", "No.", "Nope", "nope","Nope.", "nope.", "no", "no.", "No thank you", "No thank you.",
                           "no thank you", "no thank you.","No, thank you", "No, thank you.",
                           "no, thank you", "no, thank you.", "Nothing", "ty", "nothing", "not really", "Not really", "Ni", "ni", 
                           " No", "Not at this time", "No, thx", "Nope !", "NONE", "Nope!", "not at this time", 
                           "Not at the moment", "No thanks", "no thanks", "Nothing I can think of.", "Nothing I can think of.",
                           "^no[.]{0-6}", "^No[.]{0-6}", "Not right now", "not right now", "I do not")))


# all data from Q1 --------------------------------------------------------

open_ended1 = open_ended %>%
  filter(question == "01") 

# do we need this?
open_tidy1 = open_ended1 %>%
  unnest_tokens(output = word, input = response, token = "words") %>%
  anti_join(get_stopwords()) %>%
  filter(!str_detect(word, "[0-9]+")) 

# processing

processed1 <- textProcessor(open_ended1$response, metadata = open_ended1)
out1 <- prepDocuments(processed1$documents, processed1$vocab, processed1$meta)
docs1 <- out1$documents
vocab1 <- out1$vocab
meta1 <- out1$meta

heldout1 = make.heldout(docs1, vocab1)

set.seed(07312020)
plan(multiprocess) # parallel processing -- don't run this if you want to keep using your laptop

many_models1 <- tibble(K = c(2, 3, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90)) %>%
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

save(many_models1, k_result1, file = here("../../Data Management R3/R Data/open1_many_models.Rdata")) # save it somewhere

# model diagnostics

k_result1 %>%
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
       title = "Model diagnostics by number of topics") 

# repeat extraction with fewer topics and add some groups

many_models_1.2 <- tibble(K =seq(25,35, by=1)) %>%
  mutate(topic_model = future_map(K, ~stm(heldout1$documents, 
                                          heldout1$vocab, 
                                          K = .,
                                          verbose = FALSE)))

k_result1.2 <- many_models1.2 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, heldout1$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout1$missing),
         residual = map(topic_model, checkResiduals, heldout1$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result1.2 %>%
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
       title = "Model diagnostics by number of topics") 

# extract the final solution (30 in this case)

stm_q1<- stm(documents = docs1, 
                vocab = vocab1,
                K = 30, 
                prevalence =~ black + poverty150 + s(Week),
                data = meta1, 
                init.type = "Spectral")


# common words by topic figure ------------------------------------------------------

# what are the most common words for each topic

beta_matrix <- tidy(stm_q1)

top_terms <- beta_matrix %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols = c(terms))

gamma_matrix <- tidy(stm_q1, matrix = "gamma")

gamma_terms <- gamma_matrix %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  scale_y_continuous(limits =c(0, .10))+
  coord_flip() +
  #theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics in 30 topic solution",
       subtitle = "With the top words that contribute to each topic") 


# identify topics ---------------------------------------------------------

labelTopics(stm_q1, 
            topics = 27) # which topic
findThoughts(stm_q1, 
             texts = out1$meta$response, 
             topics = 27, # which topic
             n = 5) # how many thoughts


# correlations between topics ---------------------------------------------

plot(topicCorr(stm_q1))



# effects -----------------------------------------------------------------

predict_30 <- estimateEffect(formula = 1:30 ~ black + poverty150 + s(Week, df = 6),  
                             stmobj = stm_q1, 
                             metadata = out1$meta, 
                             uncertainty = "Global")


# dashboard ---------------------------------------------------------------


# https://cran.r-project.org/web/packages/stminsights/vignettes/intro.html


# resources ---------------------------------------------------------------

# https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf
# https://juliasilge.com/blog/evaluating-stm/
# https://github.com/szeyuhninawang/sips-text-analysis
