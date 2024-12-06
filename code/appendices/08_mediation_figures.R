d1 <- DiagrammeR::grViz("
digraph {
  graph []
  node [shape = plaintext]
    X [label ='Duty', shape = box, color = black]
    Y [label ='Proportion acreage planted to organic seed', shape = box, color = black]
    M [label ='Farm size', shape = box]
  edge [minlen = 2]
    X->M  
    M->Y 
    X->Y [color = black]
  { rank = same; X; Y; M }
}
")
htmlwidgets::saveWidget(d1, "figures/append_med1.html")
webshot::webshot("figures/append_med1.html", "figures/append_med1.png", vheight = 300)

d2 <- DiagrammeR::grViz("
digraph {
  graph []
  node [shape = plaintext]
    X [label ='Farm size', shape = box, color = black]
    Y [label ='Proportion acreage planted to organic seed', shape = box, color = black]
    M [label ='Ability', shape = box]
  edge [minlen = 2]
    X->M  
    M->Y 
    X->Y [color = black]
  { rank = same; X; Y; M }
}
")

htmlwidgets::saveWidget(d2, "figures/append_med2.html")
webshot::webshot("figures/append_med2.html", "figures/append_med2.png", vheight = 300)


