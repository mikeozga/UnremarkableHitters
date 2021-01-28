# Determining the most Remarkable of the most unremarkable Hitters



library(tidyverse)
library(dplyr)
library(ggplot2)
library(Lahman)
library(ggthemes)
library(ggrepel)


get_wrc_plus <- function(wrc){
  HC %>%
    filter(wRCplus == wrc) %>%
    select(Name, wRCplus, WAR, wOBA, PA) %>%
    arrange(desc(WAR))
}
UH <-get_wrc_plus(100)


## ------------------------------------------------------------------------
get_birthyear <- function(Name) {
  Names <- unlist(strsplit(Name, " "))
  Master %>%
    filter(nameFirst == Names[1],
           nameLast == Names[2])  %>%
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Player = paste(nameFirst, nameLast)) %>%
    select(playerID, Player, birthyear)
}

##


PlayerInfo <- bind_rows(get_birthyear("Bob O'Farrell"),
                        get_birthyear("Johnny Ray"),
                        get_birthyear("Davee Bancroft"),
                        get_birthyear("Billy O'Brien"),
                        get_birthyear("Johnny Kling"),
                        get_birthyear("Clyde Engle"),
                        get_birthyear("Austin Jackson"),
                        get_birthyear("Brett Lawrie"),
                        get_birthyear("Jon Jay"),
                        get_birthyear("John Hummel"),
                        get_birthyear("Dave Altizer"),
                        get_birthyear("Marty McManus"),
                        get_birthyear("Mike Lieberthal"),
                        get_birthyear("John Kerins"),
                        get_birthyear("Ricky Jordan"),
                        get_birthyear("DJ LeMahieu"),
                        get_birthyear("Hugh High"),
                        get_birthyear("Juan Samuel"),
                        get_birthyear("Mark Loretta"),
                        get_birthyear("Red Killefer"),
                        get_birthyear("Randy Winn"),
                        get_birthyear("Carlos Ruiz"),
                        get_birthyear("Hank Blalock"),
                        get_birthyear("Randy Bush"),
                        get_birthyear("Walt Wilmot"),
                        get_birthyear("Steve Balboni"),
                        get_birthyear("Tom Grieve"),
                        get_birthyear("Tony Armas"),
                        get_birthyear("Jay Gibbons"),
                        get_birthyear("Jesus Guzman"),
                        get_birthyear("Jerry Turner"),
                        get_birthyear("Tony Bernazard"),
                        get_birthyear("Wes Ferrell"),
                        get_birthyear("John Coleman"),
                        get_birthyear("Dan Johnson"),
                        get_birthyear("Scott Stratton"),
                        get_birthyear("Don Hoak"),
                        get_birthyear("Dave Nicholson"),
                        get_birthyear("Billy Werber"),
                        get_birthyear("Willy Aybar"),
                        get_birthyear("Colin Moran"),
                        get_birthyear("Lew Whistler"),
                        get_birthyear("Aaron Rowand"),
                        get_birthyear("Bob Wood"),
                        get_birthyear("Tim McCarver"),
                        get_birthyear("Adam Frazier"),
                        get_birthyear("Mike Jacobs"),
                        get_birthyear("Garry Maddox"),
                        get_birthyear("Mike Moustakas"),
                        get_birthyear("Chris Ianetta"),
                        get_birthyear("Juan Francisco"),
                        get_birthyear("Verne Clemons"),
                        get_birthyear("Pat Tabler"),
                        get_birthyear("Giovanny Urshela"),
                        get_birthyear("Doggie Miller"),
                        get_birthyear("Mike Scioscia"),
                        get_birthyear("Chris Richard"),
                        get_birthyear("Amby McConnell"),
                        get_birthyear("Odell Hale"),
                        get_birthyear("Willie Kamm"),
                        get_birthyear("Garrt Anderson"),
                        get_birthyear("Jack McCarthy"),
                        get_birthyear("Freddy Parent"),
                        get_birthyear("George Browne"),
                        get_birthyear("Wally Backmon"),
                        get_birthyear("Art Fletcher"),
                        get_birthyear("Cliff Lee")
)



# read data
HC = read_csv("HittersCareer.csv")
head(HC)
colnames(HC)

# filter data for wrc+ == 100
HC %>%
  filter(wRCplus == 100, PA > 600) -> UH

# homerun leaders
HR_leaders <- UH %>%
  select(Name,G, HR) %>%
  arrange(desc(HR))

hr_gg <- ggplot(HR_leaders, aes(x=G,y = HR)) + 
  geom_point() + theme_calc() + ggtitle("Homerun Totals") + xlab("Games") + 
  geom_text_repel(data=filter(HR_leaders, HR>200),aes(G,HR, label=Name))

hr_gg
## wOBA leaders

wobaleaders <- UH %>%
  select(Name, G, WAR, wOBA) %>%
  arrange(desc(wOBA))

woba_gg <- ggplot(wobaleaders, aes(WAR, wOBA)) + 
  geom_point() + 
  ggtitle("wOBA relative to WAR") + 
  xlab("WAR") +
  ylab("wOBA") + 
  theme_economist() +
  geom_text_repel(data=filter(wobaleaders, wOBA >= 0.340 | WAR > 30),aes(WAR,wOBA, label=Name))

woba_gg


## BA/Babip

babip <- UH %>%
  select(Name, AVG, BABIP) %>%
  arrange(desc(AVG)) 
ba_gg <- ggplot(babip, aes(x=BABIP,y = AVG)) + 
  geom_point() + theme_economist() + ggtitle("Batting Average Leaders") +
  xlab("BABIP") + 
  ylab("Batting Average") + 
  geom_text_repel(data=filter(warleaders, AVG>=0.3 | BABIP > .33),aes(BABIP,AVG, label=Name))
ba_gg






# war leaders
UH %>%
  arrange(desc(WAR)) -> warleaders

war_gg <- ggplot(warleaders, aes(x=G,y = WAR)) + 
  geom_point() + theme_economist() + ggtitle("WAR Totals") + xlab("Games") + 
  geom_text_repel(data=filter(warleaders, WAR>40 | G > 2000),aes(G,WAR, label=Name))
war_gg

UH %>%
  arrange(desc(wOBA)) %>%
  head(3)
UH %>%
  arrange(desc(WAR)) -> warleaders

#war/162
UH <- UH %>%
  mutate(WAR162 = WAR / G * 162)
w162 <- UH %>%
  select(Name, WAR162) %>%
  arrange(desc(WAR162))


war162_gg <- ggplot(UH, aes(x=G,y = WAR162)) + 
  geom_point() + theme_economist() + ggtitle("WAR/162 Totals") + xlab("Games") +
  ylab("WAR / 162") +
  geom_text_repel(data=filter(UH, WAR162>4),aes(G,WAR162, label=Name))
war162_gg

# add OPS to our table
UH <- UH %>%
  mutate(OPS = OBP + SLG)

final2 <- UH %>%
  filter(Name == "Art Fletcher" | Name == "Dave Bancroft")


final2

sameteam <- UH %>%
  filter(Team != "- - -")
sameteam



