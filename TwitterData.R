###########################
#       Looking for       #
#   tweets after Gloria   #
#     04-February-2020    #
###########################


# # # # # # 
# Libraries ---------------------------------------------------------------------------------------------------------
# # # # # #

library(tidyverse)
library(tidylog)
library(rtweet)
library(httpuv)
library(ggsci)

# # # # # # # # # 
# Importing data ----------------------------------------------------------------------------------------------------
# # # # # # # # # 

# positweet <- search_tweets(q = "posidonia OR posidònia OR poseidonia OR poseidònia", n = 1000)

# positweet30day <- search_30day(q = '(posidonia OR poseidonia OR #posidonia) (Gloria OR #Gloria OR temporal OR storm OR llevantada)',
#                                n = 5000,
#                                env_name = "research")
# save(positweet30day, file = "TwitterDataRtweet/PosidoniaGloriaStorm2.Rdata")
load(file = "TwitterDataRtweet/PosidoniaGloriaStorm.Rdata")
# 
# seagrasstweet30day <- search_30day(q = '(posidonia OR poseidonia OR #posidonia OR cymodocea OR cymo OR seagrass) (Gloria OR #Gloria OR temporal OR storm OR llevantada)',
#                                    n = 5000,
#                                    env_name = "research")
# save(seagrasstweet30day, file = "TwitterDataRtweet/SeagrassGloriaStorm.Rdata")
load(file = "TwitterDataRtweet/SeagrassGloriaStorm.Rdata")

# seagrasstweet30day_media_only <- search_30day(q = 'has:media(posidonia OR poseidonia OR #posidonia OR cymodocea OR cymo OR seagrass) (Gloria OR #Gloria OR temporal OR storm OR llevantada)',
#                                     n = 5000,
#                                     env_name = "research")
# save(seagrasstweet30day_media_only, file = "TwitterDataRtweet/SeagrassMediaOnlyGloriaStorm.Rdata")
load(file = "TwitterDataRtweet/SeagrassMediaOnlyGloriaStorm.Rdata")



positweet %>%
  # filter(is_retweet == FALSE) %>% 
  group_by(is_retweet) %>% 
  ts_plot("6 hours") +
  labs(x = "Time", 
       y = "# of tweets",
       title = "Frequency of Twitter statuses from past 30 days",
       subtitle = "Twitter status (tweet) counts aggregated using 6-hour intervals\nfor the keywords posidonia and variants",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet") +
  scale_colour_d3(palette = "category20", labels = c("Tweets", "Retweets")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = c(0.8, 0.7))
# ggsave("Figures/posidonia_tweets.png")

seagrasstweet30day %>%
  # filter(is_retweet == FALSE) %>% 
  group_by(is_retweet) %>% 
  ts_plot("3 hours") +
  labs(x = "Time", 
       y = "# of tweets",
       title = "Frequency of Twitter statuses from past 30 days",
       subtitle = "Twitter status (tweet) counts aggregated using 3-hour intervals\nfor the keywords (posidonia OR poseidonia OR #posidonia OR cymodocea OR\ncymo OR seagrass) AND (Gloria OR #Gloria OR temporal OR storm OR llevantada)",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet") +
  scale_colour_d3(palette = "category20", labels = c("Tweets", "Retweets")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = c(0.8, 0.5))
# ggsave("Figures/seagrassGloriaTwitter.png")



glimpse(seagrasstweet30day_media_only)

tweets_with_media <- seagrasstweet30day_media_only %>% 
  filter(is_retweet == F) # %>%
  # filter(!is.na(media_url)) %>% 
  # select(media_url) %>% 
  # unnest(cols = "media_url")

tweets_with_media_url <- seagrasstweet30day_media_only %>% 
  filter(is_retweet == F) %>%
  filter(!is.na(media_url)) #%>%
  # select(media_url) %>% 
  # unnest(cols = "media_url")

# download.file(tweets_images$media_url[1], destfile = "DownloadedImages/1.jpg")



a <- positweet30day %>% 
  filter(screen_name == "jordifpages")


# 
# 
# 
# delta <- search_30day(q = "has:media(temporal OR storm OR llevantada OR borrasca OR tempesta) delta ebre gloria",
#                       n = 15000,
#                       env_name = "research",
#                       toDate = 202001300000)
# save(delta, file = "TwitterDataRtweet/Delta_only_media.Rdata")
load("TwitterDataRtweet/Delta_only_media.Rdata")
delta %>%
  group_by(is_retweet) %>% 
  ts_plot("1 hours") +
  labs(x = "Time", 
       y = "# of tweets",
       title = "Frequency of Twitter statuses from past three weeks",
       subtitle = "Twitter status (tweet) counts aggregated using hourly intervals\nfor the keywords storm AND delta AND ebre AND gloria",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet") +
  scale_colour_d3(palette = "category20", labels = c("Tweets", "Retweets")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = c(0.8, 0.5))
# ggsave("Figures/deltaGloriaTwitter_retweetsVStweets.png")

delta %>%
  filter(is_retweet == FALSE) %>% 
  ts_plot("1 hours") +
  labs(x = "Time", 
       y = "# of tweets",
       title = "Frequency of Twitter statuses from past 3 weeks (NO RETWEETS)",
       subtitle = "Twitter status (tweet) counts aggregated using hourly intervals\nfor the keywords storm AND delta AND ebre AND gloria",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = c(0.8, 0.5))
ggsave("Figures/deltaGloriaTwitter_onlytweets.png")

# Interesting how just a few tweets are the ones driving the spike, because they're retweets.