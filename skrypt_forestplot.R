library(ggplot2)
library(plotly)
library(forestplot)
library(readxl)
library(dplyr)
library(ggpubr)

setwd("~/ANALITYKA/APLIKACJE/FOREST_PLOT")

dane <- read_excel("dane.xlsx")

val <- dane$val
lci <- dane$lci
uci <- dane$uci
index <- dane$index
group <- dane$group
n <- dane$n
analiza <- dane$analiza

#w podziale na wielkoœæ-test-analiza


ggplot(data=dane, aes(y=index, x=val, xmin=lci, xmax=uci, shape = analiza, col = group)) +
  geom_point(aes(size =n, col = group, shape = analiza)) + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label) +
  theme_minimal() +
  geom_vline(xintercept=0.5, color="red", linetype="dashed", alpha=.5) +
  ggtitle("Forestplot COVID-19") +
  labs(size = "Wielkoœæ próby",
       shape = "Typ analizy",
       col = "Test referencyjny",
       x = "Czu³oœæ",
       y = "Publikacja")

#w podziale na wielkoœæ-test


ggplot(data=dane, aes(y=index, x=val, xmin=lci, xmax=uci, col = analiza)) +
  geom_point(aes(size =n, col = analiza)) + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label) +
  theme_minimal() +
  geom_vline(xintercept=0.5, color="red", linetype="dashed", alpha=.5) +
  ggtitle("Forestplot COVID-19") +
  labs(size = "Wielkoœæ próby",
       shape = "Typ analizy",
       col = "Test referencyjny",
       x = "Czu³oœæ",
       y = "Publikacja")

####

ggplot(data=dane, aes(y=index, x=val, xmin=lci, xmax=uci, shape = analiza, col = group)) +
  geom_point(aes(size =n, col = group, shape = analiza)) + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label) +
  theme_minimal() +
  geom_vline(xintercept=0.5, color="red", linetype="dashed", alpha=.5) +
  ggtitle("Forestplot COVID-19") +
  labs(size = "Wielkoœæ próby",
       shape = "Typ analizy",
       col = "")
  

########################
ggplot(data=dane, aes(y=index, x=val, xmin=lci, xmax=uci, shape = analiza, col = group)) +
  geom_point(aes(size =n, col = group)) + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label) +
  theme_minimal() +
  geom_vline(xintercept=0.5, color="red", linetype="dashed", alpha=.5)

ggplot(data=dane, aes(y=index, x=val, xmin=lci, xmax=uci, col = group)) +
  geom_point(aes(size =n, col = group)) + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label) +
  theme_minimal() +
  geom_vline(xintercept=0.5, color="red", linetype="dashed", alpha=.5)+
  facet_wrap(~analiza, ncol = 1) %>%
  

dane1 <- dane %>%
  group_by(index) %>%
  select(val, lci, uci, index, group, n, analiza)%>%
  arrange(val) %>%
  mutate("lp", c(1:inf))

  ggplot(dane1,aes(y=index, x=val, xmin=lci, xmax=uci, col = group)) +
  geom_point(aes(size =n, col = group)) + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label) +
  theme_minimal() +
  geom_vline(xintercept=0.5, color="red", linetype="dashed", alpha=.5)+
  facet_wrap(~analiza, ncol = 1)



ggplot(data=dane, aes(y=index, x=val, xmin=lci, xmax=uci, shape = analiza, col = group)) +
  geom_point(aes(size =n, col = group, shape = analiza)) + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label) +
  theme_minimal() +
  geom_vline(xintercept=0.5, color="red", linetype="dashed", alpha=.5)


dane %>%
  ggplot(aes(y=index,
             x=val, 
             size = n)) + 
  geom_point()
  
             
             ,
             xmin = lci, 
             xmax = uci)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label)

dane %>%
  ggplot(aes(y=index,
             x=val)) +
  geom_point() +
  geom_errorbarh(height=.1)

,
             xmin = lci, 
             xmax = uci)) +
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label)

forestplot(mean)

ggplot(data=dane, aes(y=index, x=val, xmin=lci, xmax=uci)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(dane), labels=label)