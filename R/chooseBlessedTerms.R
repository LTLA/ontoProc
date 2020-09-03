#' Choose blessed labels
#'
#' Interactively choose the blessed labels to use for label harmonization or resolution adjustment.
#' 
#' @param onto An \code{\link{ontology_index}} object containing an ontology.
#' @param ... One or more sets of (possibly named) character vectors, 
#' each of which specifies a set of terms corresponding to a subset of nodes in \code{g}.
#' @param skip.empty Logical scalar indicating whether terms with no children in \code{...} should be skipped.
#' @param n Integer scalar specifying the number of parents/children to show at each step.
#' @param saved An environment containing the results of an incomplete run through tis function.
#'
#' @return A logical vector specifying whether each term was blessed or not.
#'
#' @author Aaron Lun
#' @examples
#' library(ontoProc)
#' bfc <- BiocFileCache::BiocFileCache(ask=FALSE)
#' path <- BiocFileCache::bfcrpath(bfc, "http://purl.obolibrary.org/obo/cl.obo")
#' co <- get_ontology(path, extract_tags="everything")
#'
#' if (interactive()) {
#'     used <- chooseBlessedTerms(co, 
#'         A=sample(co$id, 20), 
#'         B=sample(co$id, 20))
#' }
#' 
#' @export
#' @importFrom igraph make_graph topo_sort neighbors 
chooseBlessedTerms <- function(onto, ..., skip.empty=TRUE, n=20, saved=NULL) {
    parents <- onto$parents
    self <- rep(names(parents), lengths(parents))
    g <- make_graph(rbind(unlist(parents), self))
    ordering <- names(topo_sort(g))

    # Counting the number of terms.
    terms <- list(...)
    if (is.null(names(terms))) {
        names(terms) <- sprintf("set%i", seq_along(terms))
    }

    all.nodes <- unique(unlist(terms))
    collated <- vector("list", length(terms))
    names(collated) <- names(terms)
    for (i in names(terms)) {
        collated[[i]] <- ifelse(all.nodes %in% terms[[i]], "x", "")
    }

    mat <- do.call(cbind, collated)
    rownames(mat) <- all.nodes

    # Setting up a persistent environment
    if (is.null(saved)) {
        saved <- new.env()
    }
    if (is.null(saved$used)) {
        saved$used <- rep(NA, length(ordering))
        names(saved$used) <- ordering
    }
   
    # Running through the terms.
    for (i in seq_along(ordering)) {
        o <- ordering[i]
        if (!is.na(saved$used[o])) {
            next
        }

        children <- setdiff(names(subcomponent(g, o, "out")), o)
        children <- intersect(children, all.nodes)
        if (skip.empty && length(children)==0L) {
            saved$used[o] <- FALSE
            next
        }

        cat(strrep("*", 80))
        cat("\n")
        cat(sprintf("Current term is %s (%s, %i/%i)\n", o, onto$name[o], i, length(ordering)))
        cat("\n")

        # Gathering all of the children.
        cat("#########################\n")
        cat("####### Children  #######\n")
        cat("#########################\n\n")

        direct.children <- names(neighbors(g, o, mode="out"))
        df <- data.frame(name=onto$name[direct.children])
        rownames(df) <- direct.children
        print(head(df, n))
        cat("\n")
        
        cat("#########################\n")
        cat("###### Descendents ######\n")
        cat("#########################\n\n")

        df <- data.frame(name=onto$name[children], mat[children,,drop=FALSE])
        rownames(df) <- children
        print(head(df, n))
        cat("\n")

        # Gathering all of the children.
        cat("#########################\n")
        cat("####### Ancestors #######\n")
        cat("#########################\n\n")

        parents <- setdiff(names(subcomponent(g, o, "in")), o)
        df <- data.frame(name=onto$name[parents], blessed=ifelse(saved$used[parents]=="y", "y", ""))
        rownames(df) <- parents
        print(head(df, n))
        cat("\n")

        repeat {
            answer <- readline(sprintf("Bless %s (%s)? [y/n]: ", o, onto$name[o]))
            if (answer=="y") {
                saved$used[o] <- TRUE
            } else if (answer=="n") {
                saved$used[o] <- FALSE
            } else {
                next
            }
            break
        }
    }

    saved$used
}
