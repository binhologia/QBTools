# ---------------------------------------------------------------------------- #
#                                                                              #
# Quantitative Biodiversity Functions Source Code                              #
#                                                                              #
# Written by: Rachel Smith                                                     #
#                                                                              #
# Last update: 2015/03/06                                                      #
#                                                                              #
# Notes: This file contains functions to calculate metrics of taxonomic and    #
#        and phlogenetic diversity                                             #
#                                                                              #
# ---------------------------------------------------------------------------- #

require("vegan")||install.packages("vegan");require("vegan)"

# PDAR: phylogenetic diversity-area relationship
# Inputs: matrix, tree 

PDAR <- function(comm, tree) {
    areas <- c()
    diversity <- c()
    num.plots <- c(2, 4, 8, 16, 32, 51)

    for (i in num.plots) {
        areas.iter <- c()
        diversity.iter <- c()

        for (j in 1:10) {
            pond.sample <- sample(51, replace=FALSE, size=i)
            area <- 0
            sites <- c()

            for (k in pond.sample) {
                area <- area + pond.areas[k]
                sites <- rbind(sites, comm[k, ])
            }

            areas.iter <- c(areas.iter, area)
            psv.vals <- psv(sites, tree, compute.var=FALSE)
            psv <- psv.vals$PSVs[1]
            diversity.iter <- c(diversity.iter, as.numeric(psv))
        }

        diversity <- c(diversity, mean(diversity.iter))
        areas <- c(areas, mean(areas.iter))
        print(c(i, mean(diversity.iter), mean(areas.iter)))
    }

    return(cbind(areas, diversity))
}
