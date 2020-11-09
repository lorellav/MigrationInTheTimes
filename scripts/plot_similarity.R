# plot_similarity.R
# 
# Author: Jonathan de Bruin
# Project: Plot time dependent Word2Vec
# Creation date: 2019-12-06
# Modification date: 2019-12-06
# 
# Description: -
#

library(tidyverse)
library(glue)

topics = c("emigration", "immigration", "migration")


custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#C3D7A4", "#52854C", # "#4a9ea1", 
                "#4E84C4", "#293352", "#CC0000", 
                "#cc6699", "#666633")


# ------------------------------------------------------------------------\
#     load data
# ------------------------------------------------------------------------\

w2v_sim = read_csv(
  "output/similarity_file.csv",
  col_types = cols(
    start_year="i",
    end_year="i"
  )
) %>% mutate(
  mid_year = start_year + 1
)

w2v_sim_plain = w2v_sim %>%
  filter(word %in% topics)


# ------------------------------------------------------------------------\
#     Vocabularies
# ------------------------------------------------------------------------\

words_all = unique(w2v_sim$word)

# ------------------------------------------------------------------------\
#     plot functions
# ------------------------------------------------------------------------\



w2v_time_plot <- function(data, topic, words=words_all){
  p = data %>% 
    filter(topic == !!topic) %>% 
    filter(word != !!topic) %>%
    filter(word %in% words) %>% 
    ggplot(aes(start_year + 1, similarity)) + 
    geom_line(aes(color=word), size = 1) + 
    ylim(0, 1) + 
    xlab("Year") + 
    ylab("Cosine similarity") + 
    ggtitle(glue("Seed term: {topic}")) + 
    theme_bw()
  
  return(p)
}

# ------------------------------------------------------------------------\
#     Export data
# ------------------------------------------------------------------------\

w2v_time_data <- function(data, topic){
  p = data %>% 
    filter(topic == !!topic) %>% 
    filter(word != !!topic) %>%
    spread(word, similarity)
  
  return(p)
}


w2v_time_data(w2v_sim, "immigration") %>% 
  write_csv("output/data_wide_immigration.csv")

w2v_time_data(w2v_sim, "emigration") %>% 
  write_csv("output/data_wide_emigration.csv")

w2v_time_data(w2v_sim, "migration") %>% 
  write_csv("output/data_wide_migration.csv")



# ------------------------------------------------------------------------\
#     Plot graphs
# ------------------------------------------------------------------------\

# --- simple ---

# for (topic in topics) {
#   p = w2v_time_plot(w2v_sim_plain, topic)
#   
#   p + ggsave(glue("output/plot_time_w2v_{topic}_simple.png"), height = 5, width = 8)
# }


# --- advanced ---
# 
# for (topic in topics) {
#   p = w2v_time_plot(w2v_sim, topic)
#   p + ggsave(glue("output/plot_time_w2v_{topic}_full.png"), height = 5, width = 8)
# }

# ------------------------------------------------------------------------\
#     Topics plots
# ------------------------------------------------------------------------\

w2v_sim %>% 
  mutate(word_topic = paste(word, topic, sep="-")) %>% 
  filter(word_topic %in% c(
    "emigration-immigration",
    "emigration-migration",
    "migration-immigration"
  )) %>% 
  ggplot(aes(start_year + 1, similarity)) + 
  geom_line(aes(color=word_topic)) + 
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  ylim(0, 1) + 
  xlab("Year") + 
  ylab("Cosine similarity") + 
  theme_bw() + 
  scale_color_manual(values = c("#52854C", "#4E84C4", "#CC0000")) +
  ggsave("output/plot_time_w2v_main_topics.png", height = 5, width = 8)

w2v_sim %>% 
  mutate(word_topic = paste(word, topic, sep="-")) %>% 
  filter(word_topic %in% c(
    "migration-emigration"
  )) %>% 
  ggplot(aes(start_year + 1, similarity)) + 
  geom_line(color="blue") + 
  # geom_smooth(aes(color=word_topic), se = FALSE, method = "loess") +
  ylim(0, 1) + 
  xlab("Year") + 
  ylab("Cosine similarity") + 
  theme_bw() + 
  ggtitle("Similarity scores for migration and emigration") +
  ggsave("output/plot_time_w2v_migration_emigration.png", height = 5, width = 8)

w2v_sim %>% 
  mutate(word_topic = paste(word, topic, sep="-")) %>% 
  filter(word_topic %in% c(
    "migration-immigration"
  )) %>% 
  ggplot(aes(start_year + 1, similarity)) + 
  geom_line(color="#CC0000") + 
  # geom_smooth(aes(color=word_topic), se = FALSE, method = "loess") +
  ylim(0, 1) + 
  xlab("Year") + 
  ylab("Cosine similarity") + 
  theme_bw() + 
  ggtitle("Similarity scores for migration and immigration") +
  ggsave("output/plot_time_w2v_migration_immigration.png", height = 5, width = 8) 

# ------------------------------------------------------------------------\
#     Special plots
# ------------------------------------------------------------------------\


# --- plots 1900-1910 ---

w2v_sim %>% 
  filter((mid_year <= 1910) & (mid_year >= 1900)) %>%
  w2v_time_plot(
    "emigration", 
    c("canada",
      "colonies",
      "colonization",
      "population",
      "promoting",
      "recruiting",
      "relief",
      "settlers",
      "unemployed",
      "labourers"
    )) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1900, 1902, 1904, 1906, 1908, 1910), 
    limits=c(1900, 1910)
  ) +
  ggsave("output/plot_time_w2v_1900-1910_emigration.png", height = 5, width = 8) 
  

w2v_sim %>% 
  filter((mid_year <= 1910) & (mid_year >= 1900)) %>%
  w2v_time_plot(
    "immigration", 
    c("aliens",
      "asiatics",
      "exclusion",
      "prohibition",
      "suppression",
      "ordinance",
      "undesirable",
      "coolies",
      "korea",
      "indiens"
    )) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1900, 1902, 1904, 1906, 1908, 1910), 
    limits=c(1900, 1910)
  ) +
  ggsave("output/plot_time_w2v_1900-1910_immigration.png", height = 5, width = 8) 


# --- plots 1920-1930 ---

w2v_sim %>% 
  filter((mid_year <= 1930) & (mid_year >= 1920)) %>%
  w2v_time_plot(
    "immigration", 
    c(
      "maritime",
      "exclusion",
      "enforcement",
      "prohibition",
      "palestine",
      "jews", 
      "aliens",
      "colonies",
      "commonwealth",
      "government"
    )) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1920, 1922, 1924, 1926, 1928, 1930), 
    limits=c(1920, 1930)
  ) +
  ggsave("output/plot_time_w2v_1920-1930_immigration.png", height = 5, width = 8) 

w2v_sim %>% 
  filter((mid_year <= 1930) & (mid_year >= 1920)) %>%
  w2v_time_plot(
    "emigration", 
    c("colonies", 
      "dominions", 
      "overseas", 
      "commonwealth", 
      "canada", 
      "recruitment", 
      "promoting", 
      "agriculture", 
      "settlers",
      "welfare")
  ) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1920, 1922, 1924, 1926, 1928, 1930), 
    limits=c(1920, 1930)
  ) +
  ggsave("output/plot_time_w2v_1920-1930_emigration.png", height = 5, width = 8) 


w2v_sim %>% 
  filter((mid_year <= 1930) & (mid_year >= 1920)) %>%
  w2v_time_plot(
    "migration", 
    c("racial", 
      "conflicts", 
      "epidemics", 
      "immigrants", 
      "welfare", 
      "palestine", 
      "culture", 
      "promoting",
      "segregation",
      "commonwealth")
  ) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1920, 1922, 1924, 1926, 1928, 1930), 
    limits=c(1920, 1930)
  ) +
  ggsave("output/plot_time_w2v_1920-1930_migration.png", height = 5, width = 8) 

# --- plots 1945-1955 ---

w2v_sim %>% 
  filter((mid_year <= 1955) & (mid_year >= 1945)) %>%
  w2v_time_plot(
    "emigration", 
    c("palestine",
      "refugees",
      "repatriation",
      "population",
      "settlers",
      "colonies",
      "resettlement",
      "minorities",
      "overpopulation",
      "plight")) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1945, 1950, 1955)
  ) +
  ggsave("output/plot_time_w2v_1945-1955_emigration.png", height = 5, width = 8) 


w2v_sim %>% 
  filter((mid_year <= 1955) & (mid_year >= 1945)) %>%
  w2v_time_plot(
    "immigration", 
    c("palestine",
      "populations",
      "jewish",
      "government",
      "agriculture",
      "customs",
      "licensing",
      "administering",
      "restricting",
      "abolition")) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1945, 1950, 1955)
  ) +
  ggsave("output/plot_time_w2v_1945-1955_immigration.png", height = 5, width = 8) 


# --- plots 1955-1985---

w2v_sim %>% 
  filter((mid_year <= 1985) & (mid_year >= 1955)) %>%
  w2v_time_plot(
    "emigration", 
    c("influx",
      "refugees",
      "unemployment",
      "population", 
      "expulsion",
      "deportations",
      "repression",
      "oppression",
      "jews",
      "persecution")) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1955, 1960, 1965, 1970, 1975, 1980, 1985)
  ) +
  ggsave("output/plot_time_w2v_1955-1985_emigration.png", height = 5, width = 8) 


# --- plots 1955-1985---

w2v_sim %>% 
  filter((mid_year <= 1985) & (mid_year >= 1955)) %>%
  w2v_time_plot(
    "immigration", 
    c("aliens",
      "abuses",
      "colonies",
      "commonwealth",
      "discriminatory",
      "expulsion",
      "enforcement",
      "terrorism",
      "stricter",
      "laws"
      )) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1955, 1960, 1965, 1970, 1975, 1980, 1985)
  ) +
  ggsave("output/plot_time_w2v_1955-1985_immigration.png", height = 5, width = 8) 


# --- plots 1985-2000 ---

w2v_sim %>% 
  filter((mid_year <= 2000) & (mid_year >= 1985)) %>%
  w2v_time_plot(
    "immigration", 
    c("asylum", 
      "criminal",
      "enforcing", 
      "extradition", 
      "humanitarian", 
      "legislation", 
      "status", 
      "stricter", 
      "violation",
      "visas"
    )) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1985, 1990, 1995, 2000)
  ) +
  ggsave("output/plot_time_w2v_1985-2000_immigration.png", height = 5, width = 8)


w2v_sim %>% 
  filter((mid_year <= 2000) & (mid_year >= 1985)) %>%
  w2v_time_plot(
    "emigration", 
    c("persecution",
      "jews",
      "deportations",
      "refugees",
      "exodus",
      "oppression",
      "fundamentalism",
      "antisemitism",
      "repression",
      "suppression"
    )) +
  # geom_smooth(aes(color=word), se = FALSE, method = "loess") +
  guides(linetype = guide_legend(title=NULL),
         color = guide_legend(title=NULL)) + 
  scale_color_manual(values = custom.col) + 
  scale_x_continuous(
    breaks=c(1985, 1990, 1995, 2000)
  ) +
  ggsave("output/plot_time_w2v_1985-2000_emigration.png", height = 5, width = 8)