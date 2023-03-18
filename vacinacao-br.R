setwd('D:/R/estudoR/vacinaCOVID')

# carregando pacotes
library(gifski)
library(dplyr)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(png)
library(ggimage)
library(readxl)

# importando dados 


db <- read_excel('D:/R/estudoR/vacinaCOVID/db2.xlsx')

View(db)

# agrupando por estado e data e criando uma nova coluna com as vacinas diarias


db <- vacinacaoBR %>% 
  group_by(uf_sigla, data) %>%
  summarise(novos_vacinados = sum(n)) %>%           # Nao usado
  ungroup() 

head(db)  

# importando as bandeiras 

bandeiras<- data.frame(estado = c("AC","AL","AM","AP","BA","CE","DF","ES",
                                  "GO","MA","MG","MS","MT","PA","PB","PE",
                                  "PI","PR","RJ","RN","RO","RR","RS","SC", 
                                  "SE","SP","TO"),
                       imagem = c("AC.png","AL.png","AM.png","AP.png","BA.png",
                                  "CE.png","DF.png","ES.png","GO.png","MA.png",
                                  "MG.png","MS.png","MT.png","PA.png","PB.png",
                                  "PE.png","PI.png","PR.png","RJ.png","RN.png",
                                  "RO.png","RR.png","RS.png","SC.png","SE.png",
                                  "SP.png","TO.png"),stringsAsFactors = F)

bandeiras$imagem <- paste0('D:/R/estudoR/vacinaCOVID/imagens/', bandeiras$imagem)
head(bandeiras)


# juntando os dois conjunto de dados

names(db)[1] <- 'estado'

db <- full_join(db, bandeiras)
head(db)

# criando um ranking de estados que mais vacinaram por dia

rank <- db %>%
  group_by(data) %>%
  mutate(rank = rank(-total)) %>%     
  filter(rank <= 10) %>%
  ungroup()


View(rank)
names(rank)

# plotando

p <- rank %>%
  ggplot(aes(rank, group = estado)) +
  geom_tile(aes(y = total/2,
                height = total,
                width = 0.7),
            alpha = 1,
            fill = 'grey',
            color = 'grey') +
  geom_image(aes(x = rank, image = imagem), y = 0,    
             size = 0.1, hjust = 0,inherit.aes = F) +
  geom_text(aes(y = total,
                label = total,
                hjust = 0),   
            colour = 'black',
            size = 7,
            fontface = 'bold') +
  geom_text(aes(y = total,
                label = estado,
                hjust = 2),   
            colour = 'black',
            size = 7,
            fontface = 'bold') +
  coord_flip(clip = 'off', expand = F) +
  scale_x_reverse() +
  guides(color = F, fill = F) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_rect(fill = "white", colour = "white"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust= 0),
        plot.caption =element_text(size=20, hjust=0.5, face="italic", color="black"),
        plot.background=element_rect(fill = "white", colour = "white"),
        plot.margin = margin(0.1,4, 4, 4, "cm"))

a <- p + transition_states(data,
                           transition_length = 1,
                           state_length = 1,
                           wrap = F) +
  view_follow(fixed_x = T) +
  labs(title = 'Rank de vacinação contra a COVID-19 \n por estado brasileiro: {closest_state}',
         caption = 'Fonte Brasil.io | Realizado por Fellipe Porto')

a

# salvando o gif

animate(a, 100, fps = 10,                              # melhor
        width = 1300, height = 1000,
        renderer = gifski_renderer('vacina.gif'))

animate(a, 100, fps = 10,                              # melhor
        width = 1300, height = 1000,
        renderer = gifski_renderer('gif.gif'))














