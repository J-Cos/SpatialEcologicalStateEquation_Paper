library(ggsignif)
library(tidyterra)
library(tidyverse)
library(terra)
library(ranger)
library(MuMIn)
library(viridis)
library(cowplot)


mapTheme<- theme_classic()+
  theme(axis.text.y   = element_text(size=10),
        axis.text.x   = element_text(size=10),
        axis.title.y  = element_text(size=10),
        axis.title.x  = element_text(size=10),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA)
  )


boxplotTheme<-theme_classic()+
    theme(
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_blank(),
        axis.text.y  = element_text(size=12, color="black"),
        axis.text.x  = element_text(size=14, color="black"),
        legend.position = "none"
    )
