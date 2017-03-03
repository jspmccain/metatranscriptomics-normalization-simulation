library(ggplot2)
library(magrittr)

#exploring the ideas of Kleiner 2017 with a simulation
#https://peerj.com/preprints/2846v1/

no_organisms <- 10

no_genes_per_organism <- 20

#equal, half abundance half low abundance, one highly abundance
organism_dominance <- "equal"

#"equal" corresponds to the vector of abundances to multiply by no organisms. each number in this vector is identical, corresponding to a uniform distribution. 
#"halfhalf" corresponds to the vecotr of abundances where half of the organisms are doubly as abundant as the second half
#"single" corresponds to the vector of abundances where one organism corresponds to 50% of the abundance, the rest are equally low abundance.
if(organism_dominance == "equal"){
  dominance_vec <- rep(100/no_organisms, no_organisms) 
} else if(organism_dominance == "halfhalf"){
  dominance_vec <- c(rep((100/no_organisms %>% round(1))*1.5, no_organisms/2), 
                    rep((100/no_organisms %>% round(1))*0.5, no_organisms/2))
} else if(organism_dominance == "single"){
  dominance_vec <- c(46, rep(6, 9))
}

dominance_vec <- dominance_vec/10

species_names_unique <- paste("species", seq(from = 1, to = no_organisms), sep = "")
species_names <- rep(species_names_unique, 20) %>% sort()

gene_ids <- rep(seq(1, no_genes_per_organism), no_organisms)


expression_values_total <- vector()
for(i in 1:no_organisms){
  #generating pseudoexpression values drawn from a Poisson distribution of two different lambdas
  expression_values_per_gene <- rpois(n = no_genes_per_organism, 
                                      lambda = c(10, 200))
  expression_values_species_i <- expression_values_per_gene*dominance_vec[i]
  expression_values_total <- c(expression_values_species_i, expression_values_total)  
}

metatrans_df <- data.frame(species_names, gene_ids, expression_values_total)


metatrans_norm <- metatrans_df %>% 
                    group_by(species_names) %>% 
                    mutate(norm_transcript = expression_values_total/sum(expression_values_total))
  

# to make into a function
# 
# simulate_metatranscript <- function(no_organisms, 
#                                     no_genes_per_organism,
#                                     expression_values_per_gene,
#                                     organism_dominance){
#   
# }
# 
