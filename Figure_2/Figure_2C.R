###############################################################################
# Code to visualize Figure Supplement 2C                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# Load the data
Quantil_final_2 <- read.csv("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Data_Figure_2C.csv", header = T)



# Creat labels for each constrain factor
C.labs <- c("No coverage reduction", "Coverage reduction = 10%")
names(C.labs) <- c("0", "0.2")

S.labs <- c("Perennial deployment", "Seasonal deployment")
names(S.labs) <- c("sesonality1", "sesonality2")

A.labs <- c("All population", "Adult")
names(A.labs) <- c("0.75", "18")

B.labs <- c("No blood clerance", "Blood clerance")
names(B.labs) <- c("0", "1")

F.labs <- c("Degree of resistance\nto the vaccine",
            "Vaccine half-life (years)\n",
            "EIR (inoculations per \n person per year)",
            "Vaccine coverage (%)\n",
            "Access to treatment (%)\n",
            "Initial vaccine efficacy (%)\n")

names(F.labs) <- c("Degree_resistance",
                   "half_life",
                   "eir",
                   "Coverage",
                   "Access",
                   "initialEfficacy")

# define the break for the y axis
break_y <- c(1, 10, 20, 29)
Label_yy <- c("Min", "", "", "Max")


# Panel 1
constant <- 2.5
P3 <- ggplot(data = Quantil_final_2[Quantil_final_2$G == "eir", ]) +
      geom_line(aes(x = Range, y = M, color = G),
                size = 3 / constant,
                alpha = 0.75) +
      geom_ribbon(aes(x = Range,
                      ymin = L,
                      ymax = U,
                      fill = G), 
                  alpha = 0.1) +
  facet_wrap(~ G,
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" ,
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, 
                                    face = "bold", 
                                    hjust = -0.2),
    axis.title.y = element_text(size = 18 / constant, face = "bold"),
    plot.title = element_text(size = 18 / constant, 
                              hjust = 0.5, 
                              face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)",
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)",
                                "Access to treatment\n(%)",
                                "Initial vaccine efficacy\n(%)",
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",      
                                "half_life",      
                                "eir",      
                                "Coverage",      
                                "Access",      
                                "initialEfficacy",      
                                "decay_efficacy")) +
  scale_fill_manual(name = "Factors:",
                    values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                    labels = c("Degree of resistance\nto the vaccine",
                               "Vaccine half-life (years)",
                               "EIR (inoculations per \n person per year)",
                               "Vaccine coverage (%)",
                               "Access to treatment\n(%)",
                               "Initial vaccine efficacy\n(%)",
                               "Decay of vaccine efficacy\nat the second dose"),
                    breaks = c("Degree_resistance",
                               "half_life",
                               "eir",
                               "Coverage",
                               "Access", 
                               "initialEfficacy",
                               "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside")

P4 <- 
  ggplot(data = Quantil_final_2[Quantil_final_2$G == "Access", ]) +
  geom_line(aes(x = Range, 
                y = M, 
                color = G),
            size = 3 / constant,
            alpha = 0.75) +
  geom_ribbon(aes(x = Range,
                  ymin = L,
                  ymax = U,
                  fill = G), 
              alpha = 0.1) +
  facet_wrap(~ G,
             scales = "free_x",
             nrow = 1,
             strip.position = "bottom" ,
             labeller = labeller(G = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread", lim = c(0, 0.22)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant,
                                    face = "bold",
                                    hjust = -0.2),
    axis.title.y = element_text(size = 18 / constant, face = "bold"),
    plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:",
                     values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                     labels = c("Degree of resistance\nto the vaccine",
                                "Vaccine half-life (years)",
                                "EIR (inoculations per \n person per year)",
                                "Vaccine coverage (%)",
                                "Access to treatment\n(%)",
                                "Initial vaccine efficacy\n(%)",
                                "Decay of vaccine efficacy\nat the second dose"),
                     breaks = c("Degree_resistance",      
                                "half_life",      
                                "eir",      
                                "Coverage",      
                                "Access",      
                                "initialEfficacy",      
                                "decay_efficacy")) +
  scale_fill_manual(name = "Factors:",
                    values = pal[c(3, 4, 6, 13, 9, 11, 1)],
                    labels = c("Degree of resistance\nto the vaccine",
                               "Vaccine half-life (years)",
                               "EIR (inoculations per \n person per year)",
                               "Vaccine coverage (%)",
                               "Access to treatment\n(%)",
                               "Initial vaccine efficacy\n(%)",
                               "Decay of vaccine efficacy\nat the second dose"),
                    breaks = c("Degree_resistance",
                               "half_life",
                               "eir",
                               "Coverage",
                               "Access", 
                               "initialEfficacy",
                               "decay_efficacy")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold"),
        strip.text.y = element_text(size = 18 / constant,
                                    color = "black",
                                    face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm")) +
  theme(strip.background = element_blank(),  strip.placement = "outside")

PLOT_C <- plot_grid(P3, P4, ncol = 1, nrow = 2, rel_widths = c(1, 1))



# Merge all the panel (A, B , C)
#####################################

# Give a title to panel B and C
title_1 <- ggdraw() + draw_label("Vaccines targeting children", 
                                 fontface = 'bold',
                                 size = 22 / constant)
title_2 <- ggdraw() + draw_label("Vaccines targeting adults",
                                 fontface = 'bold',
                                 size = 22 / constant)

# Merge panel B with its title
PLOT_B <- plot_grid(title_1, PLOT_S1, ncol = 1, nrow = 2, rel_heights = c(1, 10), labels = c("B", ""), label_size = 18 / constant, label_fontface = 2, scale = 1)

# Merge panel  C with its title
PLOT_C2 <- plot_grid(title_2, PLOT_C, ncol = 1, nrow = 2, rel_heights = c(1, 10), labels = c("B", ""), label_size = 18 / constant, label_fontface = 2, scale = 1)

# Merge panel B and C
PLOT_B_C <- plot_grid( PLOT_B, PLOT_C2, ncol = 2, rel_widths = c(0.75, 0.25), scale = 0.99)

# Add the legend to planel B and C
PLOT_B_C_leg <- plot_grid(PLOT_B_C, leg, ncol = 1, nrow = 2, rel_heights  = c(10, 1))


# Merge all with panel A
PLOT_1 <- plot_grid(PLOT_A, PLOT_B_C_leg, ncol = 1,    nrow = 2, labels = c("A", ""), label_size = 18 / constant, label_fontface = 2, scale = 0.99, rel_heights  = c(0.9, 1))

# Save
ggsave("/scicore/home/penny/masthi00/vaccine_resistance/Visalise/New_figure_clean/Figure_2.pdf", plot = PLOT_1, width = 19, height = 24, device = "pdf", units = "cm",  dpi = 300)
