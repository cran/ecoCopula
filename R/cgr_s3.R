#' Plot graph of direct species associations.
#'
#' @param x is a cgr object, e.g. from output of \code{\link{cgr}}.
#' @param P locations of graph nodes, if NULL (default) these are generated with a Fruchterman Reingold algorithm.
#' @param vary.edge.lwd is logical, TRUE will vary line width according to the strength of partial correlation, default (FALSE) uses fixed line width.
#' @param edge.col takes two colours as arguments - the first is the colour used for positive partial correlations, the second is the colour of negative partial correlations.
#' @param label is a vector of labels to apply to each variable, defaulting to the column names supplied in the data. 
#' @param vertex.col the colour of graph nodes. 
#' @param label.cex is the size of labels. 
#' @param edge.lwd is line width, defaulting to 10*partial correlation when varying edge width, and 4 otherwise. 
#' @param edge.lty is a vector of two integers specifying the line types for positive and negative partial correlations, respectively. Both default to solid lines.
#' @param ...	other parameters to be passed through to plotting gplot, in particular \code{pad}, the amount to pad the plotting range is useful if labels are being clipped. For details see the \code{\link[sna:gplot]{gplot}} help file.
#' @return a plot of species associations after accounting for the effect of all other species, positive/negative are blue/pink.
#' The matrix of node positions (\code{P}) is returned silently.
#' @seealso \code{\link{gplot}}, \code{\link{cgr}}
#' @importFrom grDevices devAskNewPage
#' @export plot.cgr
#' @export
#' @examples
#' abund <- spider$abund[,1:5]
#' spider_mod <- stackedsdm(abund,~1, data = spider$x, ncores=2) 
#' spid_graph=cgr(spider_mod)
#' plot(spid_graph, edge.col=c("forestgreen","darkorchid4"), 
#'                  vertex.col = "black",vary.edge.lwd=TRUE)
#'                  
#'\donttest{
#'library(tidyr)
#'library(tidygraph)
#'library(ggraph)
#'
#'igraph_out<-spid_graph$best_graph$igraph_out
#'
#'igraph_out %>% ggraph('fr') + # see ?layout_tbl_graph_igraph
#'    geom_edge_fan0(aes( colour = partcor, width=partcor)) +
#'    scale_edge_width(range = c(0.5, 3))+
#'    scale_edge_color_gradient2(low="#b2182b",mid="white",high="#2166ac")+
#'    geom_node_text(aes(label=name), repel = TRUE)+
#'    geom_node_point(aes(size=1.3))+
#'    theme_void() +
#'    theme(legend.position = 'none')
#'}

plot.cgr = function(x, P = NULL, 
                    vary.edge.lwd=FALSE, edge.col = c("light blue","pink"),
                    label = colnames(x$obj$fitted), vertex.col = "blue",  
                    label.cex = 0.8, edge.lwd = ifelse(vary.edge.lwd,10,4), edge.lty=c(1,1),
                    ...) {
    
    #Partial correlations
    Theta = -cov2cor(x$best_graph$prec)
    
    #graph of partial correlations
    Graph = (Theta != 0) * 1
    
    #color of lines from sign of partial correlations
    posneg = Theta
    diag(posneg) = 0
    posneg[posneg > 0] = edge.col[1]
    posneg[posneg < 0] = edge.col[2]
    
    
    #don't ask for new plot but reset globally after
    oask <- devAskNewPage()
    devAskNewPage(FALSE)
    on.exit(devAskNewPage(oask))
    
    #use algorithm to arrange graph nodes
    if (is.null(P)) {
        P = sna::gplot.layout.fruchtermanreingold(Graph, list())
    }
    
    if(vary.edge.lwd) Graph=Theta #to plot Theta if varying line width desired

    #plot graph
    sna::gplot(Graph, gmode = "graph", label = label, coord = P, 
               vertex.col = vertex.col, edge.col = posneg, 
               label.cex = label.cex, edge.lwd = edge.lwd, 
               edge.lty=edge.lty[1], edge.lty.neg=edge.lty[2], ...)
    
    #return location of nodes
    invisible(P)
}


#' Print function for cgr object
#'
#' @param x is a cgr object, e.g. from output of \code{\link{cgr}}.
#' @param ... not used
#' @seealso \code{\link{cgr}}
#' @export print.cgr
#' @export
#' @examples
#' abund <- spider$abund[,1:5]
#' spider_mod <- stackedsdm(abund,~1, data = spider$x, ncores=2) 
#' spid_graph=cgr(spider_mod)
#' print(spid_graph)
print.cgr = function (x, ...) 
{
    cat("\nCall:\n", paste(deparse(x$obj$call), sep = "\n", 
                           collapse = "\n"), "\n\n", sep = "")
   
    Theta = -cov2cor(x$best_graph$prec)
    diag(Theta)=0
    cat("Pairwise associations:\n")
    print(paste0(sum(Theta>0)," positive and ",sum(Theta<0)," negative"))
    
    cat("\n")
    invisible(x)
}

#' Summary function for cgr object
#'
#' @param object is a cgr object, e.g. from output of \code{\link{cgr}}.
#' @param ... not used
#' @seealso \code{\link{cgr}}
#' @export summary.cgr
#' @export
#' @examples
#' abund <- spider$abund[,1:5]
#' spider_mod <- stackedsdm(abund,~1, data = spider$x, ncores=2) 
#' spid_graph=cgr(spider_mod)
#' summary(spid_graph)
summary.cgr = function (object, ...) 
{
    cat("\nCall:\n", paste(deparse(object$obj$call), sep = "\n", 
                           collapse = "\n"), "\n\n", sep = "")
    
    Theta = -cov2cor(object$best_graph$prec)
    diag(Theta)=0
    colnames(Theta)=substr(colnames(Theta),1,3)
    rownames(Theta)=substr(rownames(Theta),1,3)
    
    cat("Pairwise associations:\n")
    print(round(Theta,3))
    
    cat("\n")
    invisible(object)
}
