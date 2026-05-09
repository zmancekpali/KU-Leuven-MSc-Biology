getwd()
setwd("~/")
setwd("/Users/zojamancekpali/Desktop/KU Leuven")

library(ggplot2)

#Coordinates
y_Enk <- 9 #~68 Ma — earliest diverging
y_Pyr <- 8 #early N. Hemisphere temperate
y_Arb <- 7 #Arbutoideae+Monotropoideae clade (sister)
y_Mon <- 6 #Monotropoideae (sister to Arbutoideae)
y_Vac <- 5 #~30 Ma, temperate N. America
y_Cas <- 4 #younger Cenozoic arctic-alpine
y_Har <- 3 #younger Cenozoic arctic-alpine
y_Eri <- 2 #temperate/montane radiations
y_Sty <- 1 #Australasia, ~7 Ma modern lineages

#Divergence x positions (Ma from present)
x_root <- 90 #Ericaceae begins diversifying ~90 Ma (Kriebel et al. 2023)
x_n1 <- 68 #Enkianthoideae splits ~68 Ma (Peng et al. 2025)
x_n2 <- 60 #Pyroloideae splits (estimated, early Cenozoic)
x_n3 <- 55 #Arb+Mono clade splits
x_n4 <- 45 #Monotropoideae splits from Arbutoideae
x_n5 <- 30 #Vaccinioideae ~30 Ma (Becker et al. 2024)
x_n6 <- 25 #Cassiopoideae, younger Cenozoic
x_n7 <- 20 #Harrimanelloideae, younger Cenozoic
x_n8 <- 15 #Ericoideae
x_tip <- 0 #now

#Midpoint of subtree
yn1 <- mean(c(y_Enk, y_Sty))
yn2 <- mean(c(y_Pyr, y_Sty))
yn3 <- mean(c(y_Arb, y_Sty))
yn4 <- mean(c(y_Mon, y_Sty))  #Mono is sister to Arb, they share n3
yn5 <- mean(c(y_Vac, y_Sty))
yn6 <- mean(c(y_Cas, y_Sty))
yn7 <- mean(c(y_Har, y_Sty))
yn8 <- mean(c(y_Eri, y_Sty))

#Arb+Mono share a node at x_n3, spanning y_Arb to y_Mon
yn_ArbMon <- mean(c(y_Arb, y_Mon))

#Segments
segs <- rbind(
  data.frame(x=x_n1, xend=x_tip, y=y_Enk, yend=y_Enk, lty="solid"),
  data.frame(x=x_n2, xend=x_tip, y=y_Pyr, yend=y_Pyr, lty="dashed"), #contested
  data.frame(x=x_n3, xend=x_tip, y=y_Arb, yend=y_Arb, lty="solid"),
  data.frame(x=x_n3, xend=x_tip, y=y_Mon, yend=y_Mon, lty="solid"),
  data.frame(x=x_n5, xend=x_tip, y=y_Vac, yend=y_Vac, lty="solid"),
  data.frame(x=x_n6, xend=x_tip, y=y_Cas, yend=y_Cas, lty="solid"),
  data.frame(x=x_n7, xend=x_tip, y=y_Har, yend=y_Har, lty="solid"),
  data.frame(x=x_n8, xend=x_tip, y=y_Eri, yend=y_Eri, lty="solid"),
  data.frame(x=x_n8, xend=x_tip, y=y_Sty, yend=y_Sty, lty="solid"),
  data.frame(x=x_n3, xend=x_n3, y=y_Arb, yend=y_Mon, lty="solid"), #Arb+Mono shared node vertical
  data.frame(x=x_n8, xend=x_n8, y=y_Eri, yend=y_Sty, lty="solid"), #Eri+Sty shared node vertical 
  data.frame(x=x_n1, xend=x_n1, y=y_Enk, yend=yn2,  lty="solid"),
  data.frame(x=x_n2, xend=x_n2, y=y_Pyr, yend=yn3,  lty="solid"),
  data.frame(x=x_n3, xend=x_n3, y=yn_ArbMon, yend=yn5, lty="solid"),
  data.frame(x=x_n5, xend=x_n5, y=y_Vac, yend=yn6,  lty="solid"),
  data.frame(x=x_n6, xend=x_n6, y=y_Cas, yend=yn7,  lty="solid"),
  data.frame(x=x_n7, xend=x_n7, y=y_Har, yend=yn8,  lty="solid"),
  data.frame(x=x_n8, xend=x_n8, y=mean(c(y_Eri,y_Sty)), yend=yn8, lty="solid"),
  data.frame(x=x_root, xend=x_n1, y=yn1,        yend=yn1,        lty="solid"),
  data.frame(x=x_n1,   xend=x_n2, y=yn2,        yend=yn2,        lty="solid"),
  data.frame(x=x_n2,   xend=x_n3, y=yn3,        yend=yn3,        lty="solid"),
  data.frame(x=x_n3,   xend=x_n5, y=yn5,        yend=yn5,        lty="solid"),
  data.frame(x=x_n5,   xend=x_n6, y=yn6,        yend=yn6,        lty="solid"),
  data.frame(x=x_n6,   xend=x_n7, y=yn7,        yend=yn7,        lty="solid"),
  data.frame(x=x_n7,   xend=x_n8, y=yn8,        yend=yn8,        lty="solid"),
  stringsAsFactors = FALSE)

#Labels 
tips <- data.frame(
  label = c(
    "Enkianthoideae",
    "Pyroloideae *",
    "Arbutoideae",
    "Monotropoideae",
    "Vaccinioideae",
    "Cassiopoideae",
    "Harrimanelloideae",
    "Ericoideae",
    "Styphelioideae"),
  y = c(y_Enk, y_Pyr, y_Arb, y_Mon, y_Vac,
        y_Cas, y_Har, y_Eri, y_Sty),
  stringsAsFactors = FALSE)

#Phylogram 
(p <- ggplot() +
  geom_vline(xintercept = c(25, 50, 75),
             linetype = "dashed", color = "grey88", linewidth = 0.4) +
  geom_segment(
    data = segs,
    aes(x = x, xend = xend, y = y, yend = yend, linetype = lty),
    color = "#222222", linewidth = 1.2) +
  scale_linetype_identity() +
  geom_text(data = tips,
            aes(x = -1, y = y + 0.22, label = label),
            hjust = 1, fontface = "bold", size = 3.5, color = "black") +
  scale_x_continuous(
    name   = "Million years ago (Ma)",
    breaks = c(0, 25, 50, 75, 90),
    labels = c("0", "25", "50", "75", "90"),
    limits = c(-68, 95),
    expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.4, 9.6), expand = c(0, 0)) +
  theme_classic() +
  theme(plot.caption = element_text(size = 7.5, color = "grey40", hjust = 0, margin = margin(t = 6)),
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 10, margin = margin(t = 6)),
    axis.text.x = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10)))

ggsave("ericaceae_phylogram.png", plot = p,
       width = 13, height = 6.5, units = "in", dpi = 300)

