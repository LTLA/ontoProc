% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getOntos.R
\name{getCellOnto}
\alias{getCellOnto}
\alias{getCellLineOnto}
\alias{getEFOOnto}
\alias{getChebiLite}
\alias{getCellosaurusOnto}
\alias{getUBERON_NE}
\alias{getChebiOnto}
\alias{getOncotreeOnto}
\alias{getDiseaseOnto}
\alias{getGeneOnto}
\alias{getHCAOnto}
\alias{getPROnto}
\alias{getPATOnto}
\title{load ontologies that may include non-ascii strings and therefore cannot be in data folder}
\usage{
getCellOnto(useNew = TRUE)

getCellLineOnto()

getEFOOnto()

getChebiLite()

getCellosaurusOnto()

getUBERON_NE()

getChebiOnto()

getOncotreeOnto()

getDiseaseOnto()

getGeneOnto()

getHCAOnto()

getPROnto()

getPATOnto()
}
\arguments{
\item{useNew}{logical(1) only for getCellOnto if TRUE cell ontology of July 2018, otherwise use legacy}
}
\value{
instance of ontology_index (S3) from ontologyIndex

instance of ontology_index (S3) from ontologyIndex
}
\description{
load ontologies that may include non-ascii strings and therefore cannot be in data folder
}
\note{
Provenance information is kept in the form
of excerpts of top records in `dir(system.file("obo", package="ontoProc"), full=TRUE)`

getChebiOnto loads ontoRda/chebi_full.rda

getOncotreeOnto loads ontoRda/oncotree.rda

getDiseaseOnto loads ontoRda/diseaseOnto.rda

getHCAOnto loads ontoRda/hcaOnto.rda produced from hcao.owl at https://github.com/HumanCellAtlas/ontology/releases/tag/1.0.6 2/11/2019,
python pronto was used to convert OWL to OBO.

getPROnto loads ontoRda/PRonto.rda, produced from http://purl.obolibrary.org/obo/pr.obo 'reasoned' ontology from OBO foundry, 02-08-2019.
In contrast to other ontologies, this is imported via get_OBO with
`extract_tags='minimal'`.

getPATOnto loads ontoRda/patoOnto.rda, produced from https://raw.githubusercontent.com/pato-ontology/pato/master/pato.obo from OBO foundry, 02-08-2019.
}
\examples{
co = getCellOnto(useNew=TRUE)
co
clo = getCellLineOnto()
length(clo$id)
che = getChebiLite()
length(che$id)
efo = getEFOOnto()
length(efo$id)
}
