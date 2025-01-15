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

palette_models1 <- group.colors <- c(EEOS = "#154360", MLM="#FF5733", LM="#FFC300", Productivity="#1ABC9C")
palette_models2 <- group.colors <- c(B_predicted = "#154360", B_rf="#FF5733", B_lm="#FFC300", e="#1ABC9C")
palette_models3 <- group.colors <- c(t = "#154360", t_rf="#FF5733", t_lm="#FFC300", t_mte="#1ABC9C")