getwd()
setwd('/Users/kierstenhileman/Desktop/Data Analysis')
library(haven)
library(tidyverse)
library(scales)
library(ggplot2)
library(patchwork)
library(dplyr)
library(ggplot2)
install.packages('ggrepel')
library(ggrepel)
list.files()
df = read.csv('CTHI_2021_Main_Results.csv')
df1 = read.csv('fsi_2022_main_results.csv') 
plot(df$rank, df$Haven.Score)
p <- ggplot(data = df, aes(x = Rank, y = Haven.Score, label = ISO.2)) +
  geom_point(color = case_when(
             df$Rank >= 65 ~ 'dodgerblue',
             df$Rank <= 5 ~ 'forestgreen',
             TRUE ~ 'slateblue1'), 
               size = 3, alpha = 0.8) +
  geom_text_repel(data          = subset(df, Rank >= 65),
                  nudge_y       = 32 - subset(df, Rank >= 65)$Rank,
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "grey50",
                  direction     = "x") +
  geom_text_repel(data          = subset(df, Rank <= 5),
                  nudge_y       = 32 - subset(df, Rank <= 5)$Rank,
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "grey50",
                  direction     = "x")
p <- p + labs(title =  "Country Tax Haven Rankings", x = "Country Rank", y = "Tax Haven Score")
p <- p + theme(plot.title  = element_text(hjust = 0.5, vjust = .1),
               text=element_text(size=16,  family="serif"), 
               panel.grid.major.y = element_line(color = 'salmon'),
               panel.grid.major.x = element_blank(),
               axis.ticks.x.bottom = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.x = element_blank(),
               panel.background = element_rect(fill='transparent'))

plot(p)


ggsave(
  'Taxen_Haven.jpeg',
  plot = p,
  width = 6, height = 3.5,
  dpi = 400
)

colnames(df1)[2] = 'ISO.2'
colnames(df1)[13] = 'FSI Rank'
colnames(df3)[21] = 'FSI Score'
df2 <- full_join(df, df1,
          by = join_by("ISO.2"))
na.omit(df2)
df3 <- df2 %>% distinct()

g <- ggplot(data = df3, aes(x = `FSI Rank`, y = `FSI Score`, label = ISO.2)) +
  geom_point(color = case_when(
    df3$`FSI Rank` >= 135 ~ 'dodgerblue',
    df3$`FSI Rank` <= 5 ~ 'forestgreen',
    TRUE ~ 'slateblue1'), 
    size = 1, alpha = 0.8) +
  geom_text_repel(data          = subset(df3, `FSI Rank` >= 137),
                  nudge_y =     500 - subset(df3, `FSI Rank` >= 137)$`FSI Rank`,
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 50,
                  segment.size  = 0.2,
                  segment.color = "dodgerblue",
                  direction     = "x") +
  geom_text_repel(data          = subset(df3, `FSI Rank` <= 5),
                  nudge_y       = 500 - subset(df3, `FSI Rank` <=5)$`FSI Rank`,
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "forestgreen",
                  direction     = "x") +
  scale_x_continuous(
    limits = c(0, 150, 50),
    breaks = c(30, 60 , 90, 120, 150))
options(ggrepel.max.overlaps = 15)  
    
g <- g + labs(title =  "Country Secrecy Ranking", y = "Secrecy Score", x = "Country")
g <- g + theme(plot.title  = element_text(hjust = 0.5, vjust = .1),
            axis.title.x = element_blank(),
            panel.grid.major.x = element_line(color = 'mistyrose'),
            panel.background = element_rect(fill='transparent'),
            text=element_text(size=16,  family="serif"))
plot(g)

ggsave(
  'Country_Secrecy.jpeg',
  plot = g,
  width = 8, height = 6,
  dpi = 400
)

