library("cowplot")

#creation of panel figure for paper!
ROW1 <- plot_grid(M3.1,"", M4.1, align = "h", axis = "l", ncol =3, rel_widths = c(1,.25,1))
ROW2 <- plot_grid(M3.2,"",M4.2, align = "h", axis = "l", ncol =3, rel_widths = c(1,.25,1))
ROW3 <- plot_grid(M3.3,"",M4.3, align = "h", axis = "l", ncol =3, rel_widths = c(1,.25,1))
ROW4 <- plot_grid(M3.4,"",M4.4, align = "h", axis = "l", ncol =3, rel_widths = c(1,.25,1))
ROW5 <- plot_grid(M3.5,"",M4.5, align = "h", axis = "l", ncol =3, rel_widths = c(1,.25,1))
ROW6 <- plot_grid(M3.6,"",M4.6, align = "h", axis = "l", ncol =3, rel_widths = c(1,.25,1))
ROW7 <- plot_grid(M3.7,"",M4.7, align = "h", axis = "l", ncol =3, rel_widths = c(1,.25,1))

plot_grid(ROW1,ROW2,ROW3,ROW4,ROW5,ROW6,ROW7, ncol = 1, align = "h")
