#' Differential DNA Methylation Overrepresentation Analysis
#'
#' A function that generates a plot summarizing the findings of an
#' overrepresentation analysis performed using the driver genes derived from
#' DNA methylation samples.
#'
#' @param methylationCancer Matrix containing the methylation data
#' belonging to a sample representative of disease being studied
#' @param methylationNormal Matrix containing the methylation data
#' belonging to a sample representative of baseline methylation
#' @param geneCancer Matrix containing the gene expression data belonging to
#' a sample representative of the disease being studied
#' @param pValue double representing the cutoff for how low a p-value must be
#' for a biological pathway to be considered enriched
#' @param qValue double representing the cutoff for how low a q-value must be
#' for a biological pathway to be considered enriched
#'
#' @return Returns a bar plot that summarizes the findings of the
#' overrepresentation analysis performed using the driver genes derived from
#' DNA methylation samples
#'
#' @examples
#' library(MethylMix)
#' data(METcancer)
#' data(METnormal)
#' data(GEcancer)
#' resPlot <- methylOverrep(METcancer, METnormal, GEcancer, 1, 1)
#' resPlot
#'
#' @references
#' Carlson M (2022). org.Hs.eg.db: Genome wide annotation for Human. R package
#' version 3.15.0.
#'
#' Gevaert O (2022). MethylMix: Identifying methylation driven cancer genes. R
#' package version 2.26.0.
#'
#' R Core Team (2022). R: A language and environment for statistical computing.
#' R Foundation for Statistical Computing, Vienna, Austria.
#' URL https://www.R-project.org/.
#'
#' T Wu, E Hu, S Xu, M Chen, P Guo, Z Dai, T Feng, L Zhou, W Tang, L Zhan,
#' X Fu, S Liu, X Bo, and G Yu. clusterProfiler 4.0: A universal enrichment
#' tool for interpreting omics data. The Innovation. 2021, 2(3):100141
#'
#' @export
#' @import MethylMix
#' @import clusterProfiler
#' @import graphics
#' @import org.Hs.eg.db
methylOverrep <- function(methylationCancer,
                          methylationNormal,
                          geneCancer,
                          pValue,
                          qValue) {

  if (dim(geneCancer)[1] * dim(geneCancer)[2] == 0 ||
      dim(methylationCancer)[1] * dim(methylationCancer)[2] == 0 ||
      dim(methylationNormal)[1] * dim(methylationNormal)[2] == 0) {
    stop("Inputs containing methylation and gene samples must not be empty")
  }

  # Determining driver genes for methylation in input samples
  methylMixRes <- MethylMix::MethylMix(METcancer = methylationCancer,
                                       METnormal = methylationNormal,
                                       GEcancer = geneCancer)

  driverGenes <- methylMixRes$MethylationDrivers

  # Performing enrichment analysis
  enrichRes <- clusterProfiler::enrichGO(gene = driverGenes,
                            universe = row.names(geneCancer),
                            OrgDb = "org.Hs.eg.db",
                            keyType = 'SYMBOL',
                            readable = T,
                            ont = "BP",
                            pvalueCutoff = pValue,
                            qvalueCutoff = qValue)

  # Preventing error caused by using input with no height for barplot
  if (dim(enrichRes)[1] == 0) {
    stop("No biological processes were found to be significantly enriched")
  }

  return(barplot(enrichRes,
                 title = "GO Biological Pathways",
                 font.size = 8,
                 showCategory = 10))
}
# [END]
