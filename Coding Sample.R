#Please install these packages if you do not have them and 
#would like to replicate the code
#If asked to update packages from nflscrapR and it doesn't 
#update properly, redownload and select no update

#Use Control + Shift + C to block comment again when done

install.packages("UpSetR")
install.packages("devtools")
devtools::install_github(repo = "maksimhorowitz/nflscrapR")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("na.tools")
install.packages("ggplot2")
install.packages("pander")
install.packages("ggimage")
install.packages("ggrepel")
install.packages("teamcolors")


#Load Packages Here
library(UpSetR)
library(devtools)
library(nflscrapR)
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggplot2)
library(pander)
library(ggimage)
library(ggrepel)
library(teamcolors)

#Note: This sample of code was made with assistance from 
#NFLScrapR Github Repository
#https://github.com/ryurko/nflscrapR-data

pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))

#review play by play head for 2018
pbp %>% select(posteam, defteam, desc, play_type) %>% head

#remove plays all plays that are not run, pass, or no plays
pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

#Assign rush and pass attempts based of descriptions for these no plays
#with new variables
pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

#Filter out these plays now
pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)



pbp_players <- pbp_rp %>% 
  mutate(
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name)
  )

#End Assisted Section

#Our goal for this section is to find expected points added
#for running backs when in receiving situations

#Group eligible players by expected points added rushing
rushers<- pbp_rp %>%
  filter(rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>60)
head(rushers)

#Group eligible players by expected points added receiving
rb_passcatchers<- pbp_rp %>%
  group_by(receiver_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>20)
head(rb_passcatchers)

#Use column bind to setup dataframe
jk <- do.call(cbind.data.frame, rb_passcatchers)
head(jk)

#Filter Data for rusher names
kj <- rb_passcatchers[[1]][(rb_passcatchers[[1]]%in% rushers[[1]])]
head(kj)
counter=1

df_newjk<- data.frame()

#Filter out NAs
kj<-kj[!is.na(kj)]
kj


#Create new data frame with eligible pass catching RBs through 
#filtering data


while(counter<(length(kj)+1)){
    if (is.na(jk$receiver_player_name[1])==TRUE){
      jk<-jk[-c(1),]
    }
    if (kj[counter]==jk$receiver_player_name[1]){
      df_new22<-do.call(cbind.data.frame, jk[1,]);
      df_newjk<-do.call(rbind.data.frame, list(df_newjk, df_new22));
      counter= counter + 1;
    }
    else{
      jk<-jk[-1,];
    }
  }

#Check Dataframe
head(df_newjk)

#Should be just runningbacks with their receiving EPA

#Plot data using ggplot
df_newjk %>%
  ggplot(aes(x=success_rate, y=mean_epa)) + 
  geom_point(color = 'red')+
  #geom_image(aes(image = url), size = 0.05) +
  geom_text_repel(aes(label=receiver_player_name),
                  size=2.5,force=1, point.padding=0.1,
                  segment.size=0.2) +
  labs(x = "Success Rate",
       y = "Epa/play",
       caption = "Data from nflscrapR",
       title = "RBs Target Efficiency",
       subtitle = "RBs",
       color = "Legend Title\n") +
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))


#Multiple Seasons now, assisted from earlier section
#This may take a few minutes to load, apologies

first <- 2009 #first season to grab. min available=2009
last <- 2018 # most recent season

datalist = list()
for (yr in first:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards) %>% select(-blocked_player_id) %>% select(-fumble_recovery_2_player_id)
  datalist[[yr]] <- pbp # add it to your list
}

pbp_all <- dplyr::bind_rows(datalist)


pbp_all %>% group_by(home_team) %>%summarize(n=n(), seasons=n_distinct(season), minyr=min(season), maxyr=max(season)) %>% 
  arrange(seasons)

#Rename Teams
pbp_all <- pbp_all %>% 
  mutate_at(vars(home_team, away_team, posteam, defteam), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  ))) 

pbp_all %>% group_by(home_team) %>%summarize(n=n(), seasons=n_distinct(season), minyr=min(season), maxyr=max(season)) %>% 
  arrange(seasons)

#Option to save data so you dont have to redownload, simply
#uncomment and run
#saveRDS(pbp_all, file="pbpMulti.rds")

#Load here if you saved
#If you have trouble saving, navigate to session, set working
#directory, choose directory, and set it to wherever the file is
#stored
#pbp_all <- readRDS("pbpMulti.rds")


pbp_all_rp <- pbp_all %>%
  filter(!is_na(epa), !is_na(posteam), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0),
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		      (up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
    play=1
  ) %>%
  filter(pass==1 | rush==1)

#End assisted section

#Here is an LA themed section for fun
#Our goal here is to compare receiver EPAs (RBs and TEs
#included) in the Fisher era vs the McVay era
#Note, this does not include the most recent season, I plan to
#update when full season data is confirmed in


#Start of Fisher vs Mcvay, make sure appropriate data is loaded
#Move players into dataframe by year, get receiving data from 
#2012 through 2018
df2<-data.frame()
counter=2012
while(counter<2019){
  holder_new<- pbp_all_rp %>%
    filter(posteam == "LA", pass == 1& !is.na(receiver_player_name), down<=2, season==counter) %>%
    group_by(receiver_player_name) %>%
    summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), max_epa = max(epa), plays=n(), year=counter) %>%
    arrange(desc(mean_epa)) %>%
    filter(plays>20);
  df_new<-do.call(cbind.data.frame, holder_new)
  df2<-do.call(rbind.data.frame, list(df2, df_new))
  counter= counter+1}

#Filter NAs
df2<-df2[complete.cases(df2[ , 1]),]
head(df2)

#Transform each column to numeric
transform(df2, mean_epa = as.numeric(mean_epa))
transform(df2, success_rate = as.numeric(success_rate))
transform(df2, plays = as.numeric(plays))
transform(df2, year = as.numeric(year))

#Plot Data with ggplot
#Fisher is in blue, McVay in yellow
#I chose not to add a key as this graph is not a finished 
#product and it throws off the picture on most computers
df2 %>%
  ggplot(aes(x=success_rate, y=mean_epa)) + 
  geom_point(color = ifelse(df2$year >= 2017, "yellow", "blue"))+
  #geom_image(aes(image = url), size = 0.05) +
  geom_text_repel(aes(label=receiver_player_name),
                  size=2.5,force=1, point.padding=0.1,
                  segment.size=0.2) +
  labs(x = "Success Rate",
       y = "Epa/play",
       caption = "Data from nflscrapR",
       title = "Rams Early-down Receiving EPA/ Success Rate",
       subtitle = "2012-2018",
       color = "Legend Title\n") +
  scale_color_manual(labels = c("Fisher", "McVay"), values = c("blue", "yellow"))+
  theme_bw() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))



