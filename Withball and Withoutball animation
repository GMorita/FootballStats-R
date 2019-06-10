library(tidyverse)
library(ggsoccer)
library(gganimate)
library(gifski)
library(png)
library(jsonlite)

events7583 <- fromJSON("/Users/moritatakeshi/R/StatsBomb for 2018WC/open-data-master/data/events/7583.json")

events7583.base <- tibble(type =events7583$type$name, 
                          team = events7583$team$name, 
                          player = events7583$player$name, 
                          location = events7583$location,
                          events7583$substitution$replacement$name
                          )

events7583.withball <- filter(events7583.base, 
                                type == "Pass" | 
                                type == "Shot" | 
                                type == "Ball Receipt*")

a <- matrix(unlist(events7583.withball$location),ncol = 2,byrow = TRUE)

events7583.withball <- add_column(events7583.withball, 
                                     location_x = a[,1],
                                     location_y = a[,2])


events7583.withoutball <- filter(events7583.base, 
                                   type == "Pressure" |
                                   type == "Ball Recoverry" | 
                                   type == "Brock" | 
                                   type == "Clearance" | 
                                   type == "Interception" |
                                   type == "Goal Keeper")  

b <- matrix(unlist(events7583.withoutball$location),ncol = 2, byrow = TRUE)

events7583.withoutball <- add_column(events7583.withoutball, 
                              location_x = b[,1],
                              location_y = b[,2])


#France withball position
events7583.withball.fa <- filter(events7583.withball, team == "Brazil")

events7583.withball.aveloc.fa <- events7583.withball.fa %>% group_by(player) %>% summarise(average_x = mean(location_x),average_y = mean(location_y))

#Select Starting eleven 
events7583.withball.aveloc.fa <- events7583.withball.aveloc.fa[events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[1]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[2]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[3]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[4]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[5]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[6]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[7]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[8]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[9]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[10]|
                                                                   events7583.withball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[11], ]

ggplot(events7583.withball.aveloc.fa) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(aes(x = events7583.withball.aveloc.fa$average_x, y = events7583.withball.aveloc.fa$average_y * -1 + 80),color = "blue") +
  geom_text(aes(x = events7583.withball.aveloc.fa$average_x, y = events7583.withball.aveloc.fa$average_y * -1 + 80, label=player),hjust=0, vjust=0) +
  theme_pitch() +
  direction_label() +
  ggtitle("Average position", 
          "Offensive event")


#Croatia withball position
events7583.withball.se <- filter(events7583.withball, team == "Mexico")

events7583.withball.aveloc.se <- events7583.withball.se %>% group_by(player) %>% summarise(average_x = mean(location_x),average_y = mean(location_y))

#Select starting eleven
events7583.withball.aveloc.se <- events7583.withball.aveloc.se[events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[1]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[2]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[3]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[4]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[5]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[6]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[7]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[8]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[9]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[10]|
                                                                   events7583.withball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[11], ]


ggplot(events7583.withball.aveloc.se) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(aes(x = events7583.withball.aveloc.se$average_x, y = events7583.withball.aveloc.se$average_y * -1 + 80),color = "red") +
  geom_text(aes(x = events7583.withball.aveloc.se$average_x, y = events7583.withball.aveloc.se$average_y * -1 + 80, label=player),hjust=0, vjust=0) +
  theme_pitch() +
  direction_label() +
  ggtitle("Average position", 
          "Offensive event")


#France withoutball position
events7583.withoutball.fa <- filter(events7583.withoutball, team == "Brazil")

events7583.withoutball.aveloc.fa <- events7583.withoutball.fa %>% group_by(player) %>% summarise(average_x = mean(location_x),average_y = mean(location_y))

#Select Starting eleven 
events7583.withoutball.aveloc.fa <- events7583.withoutball.aveloc.fa[events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[1]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[2]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[3]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[4]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[5]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[6]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[7]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[8]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[9]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[10]|
                                                                   events7583.withoutball.aveloc.fa$player == events7583$tactics$lineup[[1]]$player$name[11], ]



ggplot(events7583.withoutball.aveloc.fa) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(aes(x = events7583.withoutball.aveloc.fa$average_x, y = events7583.withoutball.aveloc.fa$average_y * -1 + 80),color = "blue") +
  geom_text(aes(x = events7583.withoutball.aveloc.fa$average_x, y = events7583.withoutball.aveloc.fa$average_y * -1 + 80, label=player),hjust=0, vjust=0) +
  theme_pitch() +
  direction_label() +
  ggtitle("Average position", 
          "Deffensive event")     



#Croatia withoutball position
events7583.withoutball.se <- filter(events7583.withoutball, team == "Mexico")

events7583.withoutball.aveloc.se <- events7583.withoutball.se %>% group_by(player) %>% summarise(average_x = mean(location_x),average_y = mean(location_y))

#Select starting eleven
events7583.withoutball.aveloc.se <- events7583.withoutball.aveloc.se[events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[1]|
                                                                        events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[2]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[3]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[4]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[5]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[6]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[7]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[8]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[9]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[10]|
                                                                         events7583.withoutball.aveloc.se$player == events7583$tactics$lineup[[2]]$player$name[11], ]
                                                                         

ggplot(events7583.withoutball.aveloc.se) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(aes(x = events7583.withoutball.aveloc.se$average_x, y = events7583.withoutball.aveloc.se$average_y * -1 + 80),color = "red") +
  geom_text(aes(x = events7583.withoutball.aveloc.se$average_x, y = events7583.withoutball.aveloc.se$average_y * -1 + 80, label=player),hjust=0, vjust=0) +
  theme_pitch() +
  direction_label() +
  ggtitle("Average position", 
          "Deffensive event")




#Animation
H1 <- transform(events7583.withball.aveloc.fa, State = c("Withball"))
H2 <- transform(events7583.withoutball.aveloc.fa, State = c("Withoutball"))

H1H2 <- rbind(H1,H2)
Hp <- ggplot(H1H2) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(aes(x = T1T2$average_x, y = T1T2$average_y * -1 + 80),color = "Yellow") +
  geom_text(aes(x = T1T2$average_x, y = T1T2$average_y * -1 + 80, label=player),hjust=0, vjust=0) +
  theme_pitch() +
  direction_label() 

Hp

Hanim <- Hp + transition_states(State,
                    transition_length = 2,
                    state_length = 1) 

A1 <- transform(events7583.withball.aveloc.se, State = c("Withball"))
A2 <- transform(events7583.withoutball.aveloc.se, State = c("Withoutball"))

A1A2 <- rbind(A1,A2)
Ap <- ggplot(A1A2) +
  annotate_pitch(dimensions = pitch_statsbomb) +
  geom_point(aes(x = T1T2$average_x, y = T1T2$average_y * -1 + 80),color = "Green") +
  geom_text(aes(x = T1T2$average_x, y = T1T2$average_y * -1 + 80, label=player),hjust=0, vjust=0) +
  theme_pitch() +
  direction_label() 

Ap

Aanim <- Hp + transition_states(State,
                                transition_length = 2,
                                state_length = 1) 


Hanim

anim_save("BrazilMexico.gif")
