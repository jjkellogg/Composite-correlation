library(colorspace) # get nice colors
library(dendextend) #for dendrogram work
library(circlize) #to make cyclic dendogram

gs <- read.csv(file.choose())
gs2 <- gs[,-c(1,2)]

rnames <- gs[,1]
botany_labels <- gs[,2] #label the data

#### Hierarchical Cluster Analysis ####
rownames(gs2) <-rnames
species_col <- rev(rainbow_hcl(3))[as.numeric(botany_labels)] #colorizes the species labels

#Calculate distance between data points
d_gs <- dist(gs2)
hc_gs <- hclust(d_gs, method = "complete")
gs_botany <- rev(levels(gs[,2]))

# Plot Dendogram
dend <- as.dendrogram(hc_gs)
dend <- color_branches(dend, k=3) #, groupLabels=botany_labels) # Color the branches based on the clusters
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(gs[,2])[order.dendrogram(dend)]
  )] # Manually match the labels, as much as possible, to the real classification of the samples:

labels(dend) <- paste(as.character(gs[,2])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "") # add the sample type to the labels
dend <- hang.dendrogram(dend,hang_height=0.1) # Hang the dendrogram
dend <- set(dend, "labels_cex", 0.5) # reduce the size of the labels

# plot:
par(mar = c(3,3,3,7))
tiff("test.tiff", units="in", width=6, height=6, res=600)
plot(dend, 
     horiz =  TRUE,  nodePar = list(cex = .007))
dev.off()

#### K-means clustering ####
library(fpc)
library(robustbase)

df <- gs2

gs2var <- df[,apply(gs2, 2, var, na.rm=TRUE) != 0] # remove unit variance columns

# do kmeans analysis
k2 <- kmeans(gs2var,           # data to be run
             centers = 2,  # number of clusters
             nstart = 25)  # number of initial configurations to test and report optimal

#Do PCA on the data
k2.pca <- prcomp(gs2var, retx=TRUE, center=F, scale=F)

#visualize clusters
fviz_cluster(k2, data = gs2var)