# From  siobhonlegan.com/posts/2022-08-03-rstats-blastn/2022-08-03-rstats-blastn.html

library(taxize)
library(tidyverse)
setwd("/Users/pedro/Documents/GitHub/Cotton-Soil-Micro/Results/blast_results/")

blastn_taxa <- read.table("./28APR2025_nt_blast_MG.tsv", sep="\t", header = FALSE)
output_columns_blast <- c("qseqid", "sseqid", "stitle", "evalue", "bitscore", "pident", "qcovs", "qcovhsp")
colnames(blastn_taxa) <- output_columns_blast

#Order table and select just the top hit
blastn_taxa_sort <- data.table::setorder(blastn_taxa, qseqid, -evalue)
blastn_taxa_top <- blastn_taxa_sort[match(unique(blastn_taxa_sort$qseqid), blastn_taxa_sort$qseqid),]

#Retrieve the genbank ID and lineage information

# get vector of unique accession numbers
unique_sseqid <- unique(blastn_taxa_top$sseqid)
unique_sseqid_df <- data.frame(GI_id = unique_sseqid)
unique_sseqid_df <- unique_sseqid_df %>%
  mutate(GI_number = str_extract(GI_id, "\\d+")) %>%
  mutate(GI_number = as.numeric(GI_number))

# search for these in the ncbi database
ncbi_lineage <- classification(genbank2uid(id = as.character(unique_sseqid_df$GI_number), verbose = TRUE), db = 'ncbi')

# merge output into a data frame
ncbi_lineage_df <-cbind(ncbi_lineage)
ncbi_lineage_df$sseqid <- unique_sseqid

# select just which lineage level to keep
ncbi_lineage_df <- dplyr::select(ncbi_lineage_df, sseqid, query, kingdom, phylum, class, order, family, genus, species)
ncbi_lineage_df <- left_join(blastn_taxa_top, ncbi_lineage_df, by = "sseqid")

# save annotated blast data frame
write.csv(ncbi_lineage_df, "./soil_16S_withtaxranks_ASV_nt_blast.csv")
