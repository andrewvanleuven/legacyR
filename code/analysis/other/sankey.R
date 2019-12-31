library(networkD3)
nodes = data.frame("name" = 
                     c("Cluster Universe", # Node 0
                       "Cluster 1", # Node 1
                       "Cluster 2", # Node 2
                       "Cluster 3", # Node 3
                       "Cluster 4", # Node 4
                       "Cluster 5", # Node 5
                       "Cluster 1", # Node 6
                       "Cluster 2", # Node 7
                       "Cluster 3", # Node 8
                       "Cluster 4", # Node 9
                       "Cluster 5", # Node 10
                       "Cluster 6", # Node 11
                       "Cluster 7", # Node 12
                       "Cluster 8", # Node 13
                       "Cluster 9", # Node 14
                       "Cluster 10",# Node 15
                       "Cluster 11",# Node 16
                       "Cluster 12",# Node 17
                       "Cluster 13",# Node 18
                       "Cluster 14",# Node 19
                       "Cluster 15"))#Node 20
links = as.data.frame(matrix(c(
  0, 1, 111,
  0, 2, 107,
  0, 3, 13,
  0, 4, 100, 
  0, 5, 23, 
  1, 6, 50, 
  1, 11, 61, 
  2, 7, 34,
  2, 10, 47,
  2, 14, 21,
  2, 18, 5,
  3, 8, 3,
  3, 13, 3,
  3, 17, 7,
  4, 9, 25,
  4, 12, 47,
  4, 16, 11,
  4, 19, 6,
  4, 20, 8,
  5, 15, 23),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankey <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)#,iterations = 0)
sankey
