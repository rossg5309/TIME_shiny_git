

#Define the theme of the graph, colors, text size, font, lines, etc
theme_graph<-theme(
  axis.ticks = element_line(colour = 'black', size = .75),
  axis.title = element_text(size = 26, face = "bold" ),
  axis.title.y = element_text(vjust = .2, hjust = .5),
  axis.title.x = element_text(colour = 'black'),
  axis.text = element_text(size = 24),
  axis.text.x = element_text(colour = 'black'),
  axis.text.y = element_text(colour = 'black'),
  axis.line.x = element_line(size = .60),
  axis.line.y = element_line(size = .60),
  
  
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.background = element_blank(),
  
  legend.text = element_text(size = 17),
  legend.title = element_text(size = 22),
  legend.key = element_rect(colour = 'black', size = 1),
  legend.position = 'top'
)
