getwd()
setwd("~/")
setwd("/Users/zojamancekpali/Desktop/KU Leuven")

# ============================================================
# Ericaceae simplified dated phylogram
# Based on Schwery et al. (2015); Pyroloideae after Liu et al. (2014)
# ============================================================
library(ape)
library(ggplot2)

# ============================================================
# 1. COORDINATES
# ============================================================
y_Enk <- 9 ; y_Eri <- 8 ; y_Sty <- 7 ; y_Har <- 6 ; y_Cas <- 5
y_Vac <- 4 ; y_Arb <- 3 ; y_Mon <- 2 ; y_Pyr <- 1

x_root <- 110
x_n1   <- 105
x_n2   <-  95
x_n3   <-  88
x_n4   <-  80
x_n5   <-  75
x_n6   <-  68
x_n7   <-  55
x_n8   <-  45
x_tip  <-   0

yn1 <- mean(c(y_Enk, y_Pyr))
yn2 <- mean(c(y_Eri,  y_Pyr))
yn3 <- mean(c(y_Sty,  y_Pyr))
yn4 <- mean(c(y_Har,  y_Pyr))
yn5 <- mean(c(y_Cas,  y_Pyr))
yn6 <- mean(c(y_Vac,  y_Pyr))
yn7 <- mean(c(y_Arb,  y_Pyr))
yn8 <- mean(c(y_Mon,  y_Pyr))

# ============================================================
# 2. BUILD SEGMENTS ONE ROW AT A TIME
# ============================================================

segs <- rbind(
  # --- Coloured horizontal branches ---
  data.frame(x=x_n1, xend=x_tip, y=y_Enk, yend=y_Enk, type="Enkianthoideae",    lty="solid"),
  data.frame(x=x_n2, xend=x_tip, y=y_Eri,  yend=y_Eri,  type="Ericoideae",        lty="solid"),
  data.frame(x=x_n3, xend=x_tip, y=y_Sty,  yend=y_Sty,  type="Styphelioideae",    lty="solid"),
  data.frame(x=x_n4, xend=x_tip, y=y_Har,  yend=y_Har,  type="Harrimanelloideae", lty="solid"),
  data.frame(x=x_n5, xend=x_tip, y=y_Cas,  yend=y_Cas,  type="Cassiopoideae",     lty="solid"),
  data.frame(x=x_n6, xend=x_tip, y=y_Vac,  yend=y_Vac,  type="Vaccinioideae",     lty="solid"),
  data.frame(x=x_n7, xend=x_tip, y=y_Arb,  yend=y_Arb,  type="Arbutoideae",       lty="solid"),
  data.frame(x=x_n8, xend=x_tip, y=y_Mon,  yend=y_Mon,  type="Monotropoideae",    lty="solid"),
  data.frame(x=x_n8, xend=x_tip, y=y_Pyr,  yend=y_Pyr,  type="Pyroloideae",       lty="dashed"),
  # --- Vertical connectors ---
  data.frame(x=x_n1, xend=x_n1, y=y_Enk, yend=yn2,  type="backbone", lty="solid"),
  data.frame(x=x_n2, xend=x_n2, y=y_Eri,  yend=yn3,  type="backbone", lty="solid"),
  data.frame(x=x_n3, xend=x_n3, y=y_Sty,  yend=yn4,  type="backbone", lty="solid"),
  data.frame(x=x_n4, xend=x_n4, y=y_Har,  yend=yn5,  type="backbone", lty="solid"),
  data.frame(x=x_n5, xend=x_n5, y=y_Cas,  yend=yn6,  type="backbone", lty="solid"),
  data.frame(x=x_n6, xend=x_n6, y=y_Vac,  yend=yn7,  type="backbone", lty="solid"),
  data.frame(x=x_n7, xend=x_n7, y=y_Arb,  yend=yn8,  type="backbone", lty="solid"),
  data.frame(x=x_n8, xend=x_n8, y=y_Mon,  yend=y_Pyr, type="backbone", lty="solid"),
  # --- Horizontal backbone ---
  data.frame(x=x_root, xend=x_n1, y=yn1, yend=yn1, type="backbone", lty="solid"),
  data.frame(x=x_n1,   xend=x_n2, y=yn2, yend=yn2, type="backbone", lty="solid"),
  data.frame(x=x_n2,   xend=x_n3, y=yn3, yend=yn3, type="backbone", lty="solid"),
  data.frame(x=x_n3,   xend=x_n4, y=yn4, yend=yn4, type="backbone", lty="solid"),
  data.frame(x=x_n4,   xend=x_n5, y=yn5, yend=yn5, type="backbone", lty="solid"),
  data.frame(x=x_n5,   xend=x_n6, y=yn6, yend=yn6, type="backbone", lty="solid"),
  data.frame(x=x_n6,   xend=x_n7, y=yn7, yend=yn7, type="backbone", lty="solid"),
  data.frame(x=x_n7,   xend=x_n8, y=yn8, yend=yn8, type="backbone", lty="solid"),
  stringsAsFactors = FALSE
)

# ============================================================
# 3. TIP LABELS
# ============================================================

tips <- data.frame(
  label = c(
    "Enkianthoideae", "Ericoideae", "Styphelioideae",
    "Harrimanelloideae", "Cassiopoideae", "Vaccinioideae",
    "Arbutoideae", "Monotropoideae", "Pyroloideae *"
  ),
  sublabel = c(
    "E. Asia · 1 genus · most basal",
    "Europe, Africa, Asia · Erica, Rhododendron",
    "Australasia · former Epacridaceae",
    "Arctic, subarctic · 1 genus (Harrimanella)",
    "Arctic, montane · 1 genus (Cassiope)",
    "Andes, SE Asia · Vaccinium · most species-rich",
    "Mediterranean, W. North America · Arbutus",
    "N. temperate · mycoheterotrophic · Monotropa",
    "Boreal · mixotrophic · 4 genera · contested rank"
  ),
  y = c(y_Enk, y_Eri, y_Sty, y_Har, y_Cas, y_Vac, y_Arb, y_Mon, y_Pyr),
  stringsAsFactors = FALSE
)

# ============================================================
# 4. COLOURS
# ============================================================

branch_colors <- c(
  "Enkianthoideae"    = "#3B6D11",
  "Ericoideae"        = "#6b4fa0",
  "Styphelioideae"    = "#BA7517",
  "Harrimanelloideae" = "#185fa5",
  "Cassiopoideae"     = "#185fa5",
  "Vaccinioideae"     = "#5F5E5A",
  "Arbutoideae"       = "#A32D2D",
  "Monotropoideae"    = "#0F6E56",
  "Pyroloideae"       = "#888780",
  "backbone"          = "#222222"
)

# ============================================================
# 5. PLOT
# ============================================================

(p <- ggplot() +
  
  geom_vline(xintercept = c(25, 50, 75, 100),
             linetype = "dashed", color = "grey88", linewidth = 0.4) +
  
  geom_segment(
    data = segs,
    aes(x = x, xend = xend, y = y, yend = yend,
        color = type, linetype = lty),
    linewidth = 1.2
  ) +
  
  scale_color_manual(values = branch_colors, guide = "none") +
  scale_linetype_identity() +
  
  geom_text(data = tips,
            aes(x = -1, y = y + 0.18, label = label),
            hjust = 1, fontface = "bold", size = 3.5, color = branch_colors) +
  
  geom_text(data = tips,
            aes(x = -1, y = y - 0.18, label = sublabel),
            hjust = 1, size = 2.8, color = "grey45") +
  
  scale_x_continuous(
    name   = "Million years ago (Ma)",
    breaks = c(0, 25, 50, 75, 100, 110),
    labels = c("0", "25", "50", "75", "100", "110"),
    limits = c(-72, 115),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0.4, 9.6), expand = c(0, 0)) +
  
  # Manual legend
  annotate("text",    x = 112, y = 9.3, label = "Biogeographic group",
           hjust = 0.5, size = 3, fontface = "bold", color = "grey20") +
  annotate("segment", x = 107, xend = 110, y = 8.8, yend = 8.8,
           color = "#3B6D11", linewidth = 1.1) +
  annotate("text", x = 111, y = 8.8, label = "E. Asian basal",
           hjust = 0, size = 2.7, color = "grey20") +
  annotate("segment", x = 107, xend = 110, y = 8.3, yend = 8.3,
           color = "#6b4fa0", linewidth = 1.1) +
  annotate("text", x = 111, y = 8.3, label = "Old World",
           hjust = 0, size = 2.7, color = "grey20") +
  annotate("segment", x = 107, xend = 110, y = 7.8, yend = 7.8,
           color = "#BA7517", linewidth = 1.1) +
  annotate("text", x = 111, y = 7.8, label = "Australasia",
           hjust = 0, size = 2.7, color = "grey20") +
  annotate("segment", x = 107, xend = 110, y = 7.3, yend = 7.3,
           color = "#185fa5", linewidth = 1.1) +
  annotate("text", x = 111, y = 7.3, label = "Arctic / subarctic",
           hjust = 0, size = 2.7, color = "grey20") +
  annotate("segment", x = 107, xend = 110, y = 6.8, yend = 6.8,
           color = "#5F5E5A", linewidth = 1.1) +
  annotate("text", x = 111, y = 6.8, label = "Core (derived)",
           hjust = 0, size = 2.7, color = "grey20") +
  annotate("segment", x = 107, xend = 110, y = 6.3, yend = 6.3,
           color = "#888780", linewidth = 1.1, linetype = "dashed") +
  annotate("text", x = 111, y = 6.3, label = "Contested rank *",
           hjust = 0, size = 2.7, color = "grey20") +
  
  labs(
    caption = paste0(
      "Based on Schwery et al. (2015). ",
      "Branch lengths approximate (divergence times from Fig.2; Schwery et al., 2015). .\n", 
      "Pyroloideae (*) elevated from tribe Pyroleae by Liu et al. (2014).",
      "Phylogenetic position of Pyroloideae partly unresolved (Rose et al., 2018)."
    )
  ) +
  
  theme_classic() +
  theme(
    plot.title   = element_text(face = "bold", size = 13,
                                margin = margin(b = 6)),
    plot.caption = element_text(size = 7.5, color = "grey40",
                                hjust = 0, margin = margin(t = 8)),
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 10, margin = margin(t = 6)),
    axis.text.x  = element_text(size = 9),
    plot.margin  = margin(10, 10, 10, 10)
  ))

print(p)

# ============================================================
# 6. SAVE
# ============================================================

ggsave("ericaceae_phylogram.pdf", plot = p,
       width = 14, height = 7, units = "in")

ggsave("ericaceae_phylogram.png", plot = p,
       width = 14, height = 7, units = "in", dpi = 300)

message("Done! Saved: ericaceae_phylogram.pdf and ericaceae_phylogram.png")


#plot
p <- ggplot() +
  
  # Guide lines
  geom_vline(xintercept = c(25, 50, 75, 100),
             linetype = "dashed", color = "grey88", linewidth = 0.4) +
  
  # Tree segments — all black
  geom_segment(
    data = segs,
    aes(x = x, xend = xend, y = y, yend = yend, linetype = lty),
    color = "#222222", linewidth = 1.2
  ) +
  scale_linetype_identity() +
  
  # Subfamily name (bold)
  geom_text(data = tips,
            aes(x = -1, y = y + 0.2, label = label),
            hjust = 1, fontface = "bold", size = 3.5, color = "black") +
  
  # Sublabel (smaller, grey)
  geom_text(data = tips,
            aes(x = -1, y = y - 0.2, label = sublabel),
            hjust = 1, size = 2.9, color = "grey45") +
  
  # Axes
  scale_x_continuous(
    name   = "Million years ago (Ma)",
    breaks = c(0, 25, 50, 75, 100, 110),
    labels = c("0", "25", "50", "75", "100", "110"),
    limits = c(-62, 112),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0.4, 9.6), expand = c(0, 0)) +
  
  theme_classic() +
  theme(
    plot.caption = element_text(size = 7.5, color = "grey40",
                                hjust = 0, margin = margin(t = 6)),
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 10, margin = margin(t = 6)),
    axis.text.x  = element_text(size = 9),
    plot.margin  = margin(10, 10, 10, 10)
  )

print(p)

# ============================================================
# 5. SAVE
# ============================================================

ggsave("ericaceae_phylogram.pdf", plot = p,
       width = 13, height = 6.5, units = "in")

ggsave("ericaceae_phylogram.png", plot = p,
       width = 13, height = 6.5, units = "in", dpi = 300)

message("Done! Saved: ericaceae_phylogram.pdf and ericaceae_phylogram.png")


#####
# ============================================================
# Ericaceae simplified dated phylogram
# Topology based on Rose et al. (2018) and Liu et al. (2014)
# Divergence times from Schwery et al. (2015), Becker et al. (2024),
# Peng et al. (2025), and Rose et al. (2018)
# ============================================================

library(ggplot2)

# ============================================================
# 1. COORDINATES
# Order top to bottom = most basal to most derived
# y=9 (top) = Enkianthoideae (most basal)
# y=1 (bottom) = Styphelioideae (most derived / youngest)
# ============================================================

y_Enk <- 9   # ~68 Ma — earliest diverging
y_Pyr <- 8   # early N. Hemisphere temperate
y_Arb <- 7   # Arbutoideae+Monotropoideae clade (sister)
y_Mon <- 6   # Monotropoideae (sister to Arbutoideae)
y_Vac <- 5   # ~30 Ma, temperate N. America
y_Cas <- 4   # younger Cenozoic arctic-alpine
y_Har <- 3   # younger Cenozoic arctic-alpine
y_Eri <- 2   # temperate/montane radiations
y_Sty <- 1   # Australasia, ~7 Ma modern lineages

# Divergence x positions (Ma from present)
# Based on text citations
x_root <- 90    # Ericaceae begins diversifying ~90 Ma (Kriebel et al. 2023)
x_n1   <- 68    # Enkianthoideae splits ~68 Ma (Peng et al. 2025)
x_n2   <- 60    # Pyroloideae splits (estimated, early Cenozoic)
x_n3   <- 55    # Arb+Mono clade splits
x_n4   <- 45    # Monotropoideae splits from Arbutoideae
x_n5   <- 30    # Vaccinioideae ~30 Ma (Becker et al. 2024)
x_n6   <- 25    # Cassiopoideae, younger Cenozoic
x_n7   <- 20    # Harrimanelloideae, younger Cenozoic
x_n8   <- 15    # Ericoideae
x_tip  <-  0    # present

# Internal node y positions (midpoint of subtree)
yn1 <- mean(c(y_Enk, y_Sty))
yn2 <- mean(c(y_Pyr, y_Sty))
yn3 <- mean(c(y_Arb, y_Sty))
yn4 <- mean(c(y_Mon, y_Sty))  # Mono is sister to Arb, they share n3
yn5 <- mean(c(y_Vac, y_Sty))
yn6 <- mean(c(y_Cas, y_Sty))
yn7 <- mean(c(y_Har, y_Sty))
yn8 <- mean(c(y_Eri, y_Sty))

# Arb+Mono share a node at x_n3, spanning y_Arb to y_Mon
yn_ArbMon <- mean(c(y_Arb, y_Mon))

# ============================================================
# 2. SEGMENTS
# ============================================================

segs <- rbind(
  
  # --- Coloured horizontal branches (node to present) ---
  data.frame(x=x_n1, xend=x_tip, y=y_Enk, yend=y_Enk, lty="solid"),
  data.frame(x=x_n2, xend=x_tip, y=y_Pyr, yend=y_Pyr, lty="dashed"),  # contested
  data.frame(x=x_n3, xend=x_tip, y=y_Arb, yend=y_Arb, lty="solid"),
  data.frame(x=x_n3, xend=x_tip, y=y_Mon, yend=y_Mon, lty="solid"),
  data.frame(x=x_n5, xend=x_tip, y=y_Vac, yend=y_Vac, lty="solid"),
  data.frame(x=x_n6, xend=x_tip, y=y_Cas, yend=y_Cas, lty="solid"),
  data.frame(x=x_n7, xend=x_tip, y=y_Har, yend=y_Har, lty="solid"),
  data.frame(x=x_n8, xend=x_tip, y=y_Eri, yend=y_Eri, lty="solid"),
  data.frame(x=x_n8, xend=x_tip, y=y_Sty, yend=y_Sty, lty="solid"),
  
  # --- Arb+Mono shared node vertical ---
  data.frame(x=x_n3, xend=x_n3, y=y_Arb, yend=y_Mon, lty="solid"),
  
  # --- Eri+Sty shared node vertical ---
  data.frame(x=x_n8, xend=x_n8, y=y_Eri, yend=y_Sty, lty="solid"),
  
  # --- Main backbone verticals ---
  data.frame(x=x_n1, xend=x_n1, y=y_Enk, yend=yn2,  lty="solid"),
  data.frame(x=x_n2, xend=x_n2, y=y_Pyr, yend=yn3,  lty="solid"),
  data.frame(x=x_n3, xend=x_n3, y=yn_ArbMon, yend=yn5, lty="solid"),
  data.frame(x=x_n5, xend=x_n5, y=y_Vac, yend=yn6,  lty="solid"),
  data.frame(x=x_n6, xend=x_n6, y=y_Cas, yend=yn7,  lty="solid"),
  data.frame(x=x_n7, xend=x_n7, y=y_Har, yend=yn8,  lty="solid"),
  data.frame(x=x_n8, xend=x_n8, y=mean(c(y_Eri,y_Sty)), yend=yn8, lty="solid"),
  
  # --- Horizontal backbone connectors ---
  data.frame(x=x_root, xend=x_n1, y=yn1,        yend=yn1,        lty="solid"),
  data.frame(x=x_n1,   xend=x_n2, y=yn2,        yend=yn2,        lty="solid"),
  data.frame(x=x_n2,   xend=x_n3, y=yn3,        yend=yn3,        lty="solid"),
  data.frame(x=x_n3,   xend=x_n5, y=yn5,        yend=yn5,        lty="solid"),
  data.frame(x=x_n5,   xend=x_n6, y=yn6,        yend=yn6,        lty="solid"),
  data.frame(x=x_n6,   xend=x_n7, y=yn7,        yend=yn7,        lty="solid"),
  data.frame(x=x_n7,   xend=x_n8, y=yn8,        yend=yn8,        lty="solid"),
  
  stringsAsFactors = FALSE
)

# ============================================================
# 3. TIP LABELS
# ============================================================

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
    "Styphelioideae"
  ),
  y = c(y_Enk, y_Pyr, y_Arb, y_Mon, y_Vac,
        y_Cas, y_Har, y_Eri, y_Sty),
  stringsAsFactors = FALSE
)

# ============================================================
# 4. PLOT
# ============================================================

p <- ggplot() +
  
  # Guide lines
  geom_vline(xintercept = c(25, 50, 75),
             linetype = "dashed", color = "grey88", linewidth = 0.4) +
  
  # Tree segments — all black
  geom_segment(
    data = segs,
    aes(x = x, xend = xend, y = y, yend = yend, linetype = lty),
    color = "#222222", linewidth = 1.2
  ) +
  scale_linetype_identity() +
  
  # Subfamily name (bold)
  geom_text(data = tips,
            aes(x = -1, y = y + 0.22, label = label),
            hjust = 1, fontface = "bold", size = 3.5, color = "black") +
  # Axes
  scale_x_continuous(
    name   = "Million years ago (Ma)",
    breaks = c(0, 25, 50, 75, 90),
    labels = c("0", "25", "50", "75", "90"),
    limits = c(-68, 95),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0.4, 9.6), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.caption = element_text(size = 7.5, color = "grey40",
                                hjust = 0, margin = margin(t = 6)),
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 10, margin = margin(t = 6)),
    axis.text.x  = element_text(size = 9),
    plot.margin  = margin(10, 10, 10, 10)
  )

print(p)

# ============================================================
# 5. SAVE
# ============================================================

ggsave("ericaceae_phylogram.pdf", plot = p,
       width = 13, height = 6.5, units = "in")

ggsave("ericaceae_phylogram.png", plot = p,
       width = 13, height = 6.5, units = "in", dpi = 300)

message("Done! Saved: ericaceae_phylogram.pdf and ericaceae_phylogram.png")
