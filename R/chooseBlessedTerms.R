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
chooseBlessedTerms <- function(onto, ..., skip.empty=TRUE, n=50, saved=NULL) {
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
    processed <- sum(!is.na(saved$used))
    for (o in ordering) {
        if (!is.na(saved$used[o])) {
            next
        }

        processed <- processed + 1L
        descendents <- setdiff(names(subcomponent(g, o, "out")), o)
        ancestors <- setdiff(names(subcomponent(g, o, "in")), o)

        observed <- intersect(descendents, all.nodes)
        if (skip.empty && length(observed)==0L && !o %in% all.nodes) {
            saved$used[o] <- FALSE
            next
        }

        cat(strrep("*", 80))
        cat("\n")
        cat(sprintf("Current term is %s (%s) (%i remaining)\n", o, onto$name[o], length(ordering) - processed + 1L))
        cat("\n")

        # Gathering all of the children.
        cat("#########################\n")
        cat("####### Children  #######\n")
        cat("#########################\n\n")

        children <- names(neighbors(g, o, mode="out"))
        df <- data.frame(name=onto$name[children])
        rownames(df) <- children
        print(head(df, n))
        if (n < nrow(df)) {
            cat(sprintf("... plus %i more term(s)\n", nrow(df) - n))
        }
        cat("\n")
        
        cat("#########################\n")
        cat("###### Descendents ######\n")
        cat("#########################\n\n")

        df <- data.frame(name=onto$name[observed], mat[observed,,drop=FALSE])
        rownames(df) <- observed 
        print(head(df, n))
        if (n < nrow(df)) {
            cat(sprintf("... plus %i more term(s)\n", nrow(df) - n))
        }
        cat("\n")

        if (any(observed==o) && !any(saved$used[ancestors])) {
            cat("NOTE: this term is directly observed but has no blessed ancestors.\n\n")
        }

        # Gathering all of the children.
        cat("#########################\n")
        cat("####### Ancestors #######\n")
        cat("#########################\n\n")

        df <- data.frame(name=onto$name[ancestors], blessed=ifelse(saved$used[ancestors], "y", ""))
        rownames(df) <- ancestors
        print(head(df, n))
        if (n < nrow(df)) {
            cat(sprintf("... plus %i more term(s)\n", nrow(df) - n))
        }
        cat("\n")

        repeat {
            answer <- readline(sprintf("Bless %s (%s)? [y/n/a/i]
  - (y)es
  - (n)o
  - bless this term for (a)ll descendents.
  - (i)gnore this term and all descendents.
Choice: ", o, onto$name[o]))
            if (answer=="y") {
                saved$used[o] <- TRUE
            } else if (answer=="n") {
                saved$used[o] <- FALSE
            } else if (answer=="a") {
                saved$used[o] <- TRUE
                saved$used[descendents] <- FALSE
                processed <- processed + length(descendents)
            } else if (answer=="i") {
                saved$used[o] <- FALSE
                saved$used[descendents] <- FALSE
                processed <- processed + length(descendents)
            } else {
                next
            }
            break
        }
    }

    saved$used
}
