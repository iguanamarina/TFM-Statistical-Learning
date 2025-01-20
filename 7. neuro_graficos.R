# install.packages(c("rgl","misc3d","neurobase"))
library(rgl);library(misc3d);library(neurobase)

if (!requireNamespace("aal")) {
  devtools::install_github("muschellij2/aal")
} else {
  library(aal)
}

if (!requireNamespace("MNITemplate")) {
  devtools::install_github("jfortin1/MNITemplate")
} else {
  library(MNITemplate)
}


library(aal); library(MNITemplate); library(neurobase); library(rgl); library(misc3d)

img = aal_image()
template = readMNI(res = "2mm")
cut <- 4500
dtemp <- dim(template)

labs = aal_get_labels()
# View(labs)



# Pick the region of the brain you would like to highlight

hippocampusL = labs$index[grep("Hippocampus_L", labs$name)]
hippocampusR = labs$index[grep("Hippocampus_R", labs$name)]

# Parietal_Inf_L = labs$index[grep("Parietal_Inf_L", labs$name)]
# Parietal_Inf_R = labs$index[grep("Parietal_Inf_R", labs$name)]
# Parietal_Sup_L = labs$index[grep("Parietal_Sup_L", labs$name)]
# Parietal_Sup_R = labs$index[grep("Parietal_Sup_R", labs$name)]

Temporal_Inf_L = labs$index[grep("Temporal_Inf_L", labs$name)]
Temporal_Inf_R = labs$index[grep("Temporal_Inf_R", labs$name)]
Temporal_Mid_L = labs$index[grep("Temporal_Mid_L", labs$name)]
Temporal_Mid_R = labs$index[grep("Temporal_Mid_R", labs$name)]
Temporal_Pole_Mid_L = labs$index[grep("Temporal_Pole_Mid_L", labs$name)]
Temporal_Pole_Mid_R = labs$index[grep("Temporal_Pole_Mid_R", labs$name)]
Temporal_Pole_Sup_L = labs$index[grep("Temporal_Pole_Sup_L", labs$name)]
Temporal_Pole_Sup_R = labs$index[grep("Temporal_Pole_Sup_R", labs$name)]
Temporal_Sup_L = labs$index[grep("Temporal_Sup_L", labs$name)]
Temporal_Sup_R = labs$index[grep("Temporal_Sup_R", labs$name)]


mask = remake_img(vec = img %in% hippocampusL, img = img)
mask2 = remake_img(vec = img %in% hippocampusR, img = img)
# mask3 = remake_img(vec = img %in% Parietal_Inf_L, img = img)
# mask4 = remake_img(vec = img %in% Parietal_Inf_R, img = img)
# mask5 = remake_img(vec = img %in% Parietal_Sup_L, img = img)
# mask6 = remake_img(vec = img %in% Parietal_Sup_R, img = img)
mask7 = remake_img(vec = img %in% Temporal_Inf_L, img = img)
mask8 = remake_img(vec = img %in% Temporal_Inf_R, img = img)
mask9 = remake_img(vec = img %in% Temporal_Mid_L, img = img)
mask10 = remake_img(vec = img %in% Temporal_Mid_R, img = img)
mask11 = remake_img(vec = img %in% Temporal_Pole_Mid_L, img = img)
mask12 = remake_img(vec = img %in% Temporal_Pole_Mid_R, img = img)
mask13 = remake_img(vec = img %in% Temporal_Pole_Sup_L, img = img)
mask14 = remake_img(vec = img %in% Temporal_Pole_Sup_R, img = img)
mask15 = remake_img(vec = img %in% Temporal_Sup_L, img = img)
mask16 = remake_img(vec = img %in% Temporal_Sup_R, img = img)


### this would be the ``activation'' or surface you want to render 
contour3d(template, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = cut, alpha = 0.4, draw = TRUE)
contour3d(mask, level = c(0.5), alpha = c(0.7), add = TRUE, color=c("lightgreen") )
contour3d(mask2, level = c(0.5), alpha = c(0.7), add = TRUE, color=c("lightgreen") )
# contour3d(mask3, level = c(0.5), alpha = c(0.5), add = TRUE, color=c("green") )
# contour3d(mask4, level = c(0.5), alpha = c(0.5), add = TRUE, color=c("green") )
# contour3d(mask5, level = c(0.5), alpha = c(0.5), add = TRUE, color=c("green") )
# contour3d(mask6, level = c(0.5), alpha = c(0.5), add = TRUE, color=c("green") )
contour3d(mask7, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask8, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask9, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask10, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask11, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask12, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask13, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask14, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask15, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )
contour3d(mask16, level = c(0.5), alpha = c(0.3), add = TRUE, color=c("lightblue") )


### add text
text3d(x=dtemp[1]/2, y=dtemp[2]/2, z = dtemp[3]*0.98, text="Top")
text3d(x=-0.98, y=dtemp[2]/2, z = dtemp[3]/2, text="Left")
rglwidget()


