## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=5,
  fig.height = 3.5,
  fig.align = 'center'
)

## ----setup, include = FALSE---------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_knit$set(global.par = TRUE)

## ----include = FALSE----------------------------------------------------------
par(mai=c(0.8,0.8,0.2,0.2))

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("ecoCopula")

## -----------------------------------------------------------------------------
library(ecoCopula)

## -----------------------------------------------------------------------------
# spider data is stored in ecoCopula
data(spider)
X <- as.data.frame(spider$x) # environmental covariates

## -----------------------------------------------------------------------------
abund <- spider$abund # abundance of spiders
abund[1:5,1:6]

## -----------------------------------------------------------------------------
pa=(abund>0)*1 # presence-absence of spiders
pa[1:4,1:6]

## -----------------------------------------------------------------------------
# fit marginal model
spider_pa <- stackedsdm(pa,~1, data = X, family="binomial",ncores = 2) #eqiv. manyglm()
# fit copula ordination 
spid_lv=cord(spider_pa)
# biplot
plot(spid_lv,biplot = TRUE)

## -----------------------------------------------------------------------------
sand<-ifelse(X$bare.sand==0,"red","blue") 
plot(spid_lv,site.col = sand)

## -----------------------------------------------------------------------------
# fit marginal model
spider_nb <- stackedsdm(abund,~., data = X, family="negative.binomial", ncores = 2) #eqiv. manyglm()
# fit copula ordination 
spid_gr=cgr(spider_nb, seed=3)
# biplot
plot(spid_gr, pad=1)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(labdsv)
# site data
data(brycesite)
brycesite$plotcode=substr(brycesite$plotcode,3,5)
#species data
data(bryceveg)
bryceveg<- bryceveg[,-which(colSums(bryceveg>0) <= 20)]  #most abundant species


## -----------------------------------------------------------------------------
#recode data to integer categories
old <- c(0,0.2,0.5,1,2,3,4,5,6) #existing categories
bryceord=bryceveg
for(i in 1:length(old)){
  bryceord[bryceveg==old[i]]=i
}
#marginal model
bryce_marg <- stackedsdm(bryceord, formula_X = ~ 1, data = brycesite, family="ordinal",ncores = 2)

## -----------------------------------------------------------------------------
bryce_ordi=cord(bryce_marg)

## -----------------------------------------------------------------------------
library(RColorBrewer)
library(ggplot2)
# data frames for plotting
site_res <- data.frame(bryce_ordi$scores,brycesite)
sp_res <- data.frame(bryce_ordi$loadings,
                     species = colnames(bryceord))
alpha= 2.5 # scaling parameter
ggplot()+
  geom_point(aes(x=Factor1,y=Factor2,color = elev ),site_res)+ #sites
  geom_text(aes(x = Factor1*alpha, y = Factor2*alpha,label = species),data=sp_res)+ #species
  scale_color_gradientn(colours = brewer.pal(n = 10, name = "PuOr"))+
  theme_classic()

## ----message=FALSE, warning=FALSE---------------------------------------------
cont_preds=sapply(brycesite, class)%in%c("int","num")
brycesite[,cont_preds]=scale(brycesite[,cont_preds])
bryce_marg_all <- stackedsdm(bryceord, formula_X = ~ slope, data = brycesite,
                             family="ordinal",ncores = 2)

bryce_graph<-cgr(bryce_marg_all, seed = 1) #seed for demonstration

## ----message=FALSE, warning=FALSE---------------------------------------------
igraph_out<-bryce_graph$best_graph$igraph_out

library(tidyr)
library(tidygraph)
library(ggraph)

igraph_out %>% ggraph('fr') + # see ?layout_tbl_graph_igraph
    geom_edge_fan0(aes( colour = partcor, width=partcor)) +
    scale_edge_width(range = c(0.5, 3))+
    scale_edge_color_gradient2(low="#b2182b",mid="white",high="#2166ac")+
    geom_node_text(aes(label=name), repel = TRUE)+
    geom_node_point(size=2)+
    theme_void() +
    theme(legend.position = 'none')


