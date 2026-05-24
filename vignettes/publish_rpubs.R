##########################################################################################
# RPubs automated publishing
#
# PHASE 1 — first time only, per document:
#   publish_first("glm-efa", slug="glm_efa")
#   This knits the file, uploads it, and POSTs the slug to RPubs automatically.
#   No browser needed if the slug is available.
#   Your document will be at: https://rpubs.com/sedzinfo/glm_efa
#
# PHASE 2 — every subsequent update (fully automatic, no browser):
#   publish_doc("glm-efa")        # one document
#   publish_all()                  # all documents
##########################################################################################

library(rsconnect)
library(rmarkdown)

RPUBS_USER <- "sedzinfo"
IDS_FILE   <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "rpubs_ids.rds")
files      <- list.files(gsub("rpubs_ids.rds","",IDS_FILE),pattern=".rmd")

DOCS<-list()
for(i in files) {
  key   <- gsub(".rmd","",i,fixed=TRUE)
  rmd   <- file.path(gsub("rpubs_ids.rds","",IDS_FILE),i)
  yml   <- tryCatch(rmarkdown::yaml_front_matter(rmd), error=function(e) list())
  title <- if (!is.null(yml$title) && nzchar(yml$title)) yml$title else key
  DOCS[[key]]<-list(rmd=i, slug=key, title=title)
}
##########################################################################################
# INTERNAL HELPERS
##########################################################################################
load_ids <- function() {
  if (file.exists(IDS_FILE)) readRDS(IDS_FILE) else list()
}

save_ids <- function(ids) {
  saveRDS(ids, IDS_FILE)
}

script_dir <- function() {
  dirname(rstudioapi::getSourceEditorContext()$path)
}

knit_to_html <- function(rmd_path) {
  html_path <- sub("\\.Rmd$", ".html", rmd_path, ignore.case=TRUE)
  cat("  Knitting:", basename(rmd_path), "...")
  rmarkdown::render(rmd_path, output_file=html_path, quiet=TRUE)
  cat(" done\n")
  html_path
}

# POST the slug to the RPubs claim endpoint.
# Returns TRUE if the slug was accepted, FALSE if it needs browser fallback.
claim_slug <- function(continue_url, slug, title) {
  if (!requireNamespace("httr", quietly=TRUE)) {
    return(FALSE)
  }
  resp <- httr::POST(
    continue_url,
    body   = list(name=slug, title=title),
    encode = "form",
    httr::config(followlocation=TRUE, maxredirs=5L)
  )
  status <- httr::status_code(resp)
  status %in% c(200L, 201L, 302L)
}

rpubs_url <- function(slug) {
  paste0("https://rpubs.com/", RPUBS_USER, "/", slug)
}
##########################################################################################
# PHASE 1: first publish
##########################################################################################

#' Publish a document for the first time.
#'
#' The slug is taken from DOCS[[key]]$slug by default.
#' Pass slug= explicitly to override, e.g.:
#'   publish_first("glm-efa", slug="glm_efa")
#'
#' The function tries to claim the URL programmatically (no browser).
#' If that fails (slug already taken, httr not installed, network error)
#' it falls back to opening the browser.
#'
#' @param key   Name from the DOCS list above
#' @param slug  RPubs slug — becomes https://rpubs.com/sedzinfo/<slug>
#' @param title Human-readable page title (defaults to DOCS entry)
publish_first <- function(key, slug=NULL, title=NULL) {
  doc   <- DOCS[[key]]
  rmd   <- file.path(script_dir(), doc$rmd)
  slug  <- if (!is.null(slug)) slug  else doc$slug
  title <- if (!is.null(title)) title else doc$title
  
  cat("\n[", key, "]\n")
  html   <- knit_to_html(rmd)
  
  cat("  Uploading HTML to RPubs...\n")
  result <- rsconnect::rpubsUpload(title=title, htmlFile=html, id=NULL)
  
  ids        <- load_ids()
  ids[[key]] <- result$id
  save_ids(ids)
  
  cat("  Claiming slug:", slug, "...\n")
  ok <- claim_slug(result$continueUrl, slug=slug, title=title)
  
  if (ok) {
    cat("  Published at:", rpubs_url(slug), "\n")
  } else {
    cat("  Automatic slug claim failed (slug taken, or httr not installed).\n")
    cat("  Opening browser — type '", slug, "' in the URL field.\n", sep="")
    cat("  Target URL will be:", rpubs_url(slug), "\n")
    browseURL(result$continueUrl)
  }
  
  invisible(list(id=result$id, url=rpubs_url(slug)))
}

##########################################################################################
# PHASE 2: update an already-published document
##########################################################################################

#' Re-knit and push an update. The RPubs URL never changes.
#'
#' @param key   Name from the DOCS list above
#' @param title Human-readable title (NULL = keep DOCS default)
publish_doc <- function(key, title=NULL) {
  ids <- load_ids()
  if (is.null(ids[[key]])) {
    stop("No ID for '", key, "'. Run publish_first('", key, "') first.")
  }
  doc   <- DOCS[[key]]
  rmd   <- file.path(script_dir(), doc$rmd)
  title <- if (!is.null(title)) title else doc$title
  
  cat("\n[", key, "]\n")
  html   <- knit_to_html(rmd)
  cat("  Updating RPubs (ID:", ids[[key]], ")...\n")
  rsconnect::rpubsUpload(title=title, htmlFile=html, id=ids[[key]])
  cat("  Updated:", rpubs_url(doc$slug), "\n")
  invisible(rpubs_url(doc$slug))
}

#' Re-knit and update ALL previously published documents.
publish_all <- function() {
  ids <- load_ids()
  if (length(ids) == 0) {
    cat("Nothing published yet. Run publish_first() for each document first.\n")
    return(invisible(NULL))
  }
  for (key in names(ids)) {
    if (key %in% names(DOCS)) {
      tryCatch(
        publish_doc(key),
        error=function(e) cat("  ERROR:", conditionMessage(e), "\n")
      )
    }
  }
  cat("\nAll documents updated.\n")
}

#' List all published documents with their URLs.
list_docs <- function() {
  ids <- load_ids()
  if (length(ids) == 0) { cat("Nothing published yet.\n"); return(invisible(NULL)) }
  cat(sprintf("%-22s  %-40s  %s\n", "Key", "URL", "ID"))
  cat(strrep("-", 90), "\n")
  for (key in names(ids)) {
    slug <- if (key %in% names(DOCS)) DOCS[[key]]$slug else "?"
    cat(sprintf("%-22s  %-40s  %s\n", key, rpubs_url(slug), ids[[key]]))
  }
}

##########################################################################################
# USAGE
##########################################################################################
#
# First publish (sets the URL slug):
#   publish_first("glm-efa")
#   publish_first("glm-efa", slug="glm_efa")                  # override slug
#   publish_first("glm-efa", slug="glm_efa", title="EFA")     # override both
#
# Update existing (no browser, URL unchanged):
#   publish_doc("glm-efa")
#   publish_all()
#
# See what's published:
#   list_docs()
#
# Expected URLs after first publish:
#   https://rpubs.com/sedzinfo/validation_ocean
#   https://rpubs.com/sedzinfo/validation_bfi44
#   https://rpubs.com/sedzinfo/glm_generalized
#   https://rpubs.com/sedzinfo/glm_efa
#   https://rpubs.com/sedzinfo/glm_anova
#   https://rpubs.com/sedzinfo/glm_means
#   https://rpubs.com/sedzinfo/glm_linear_regression
#   https://rpubs.com/sedzinfo/hlr
#   https://rpubs.com/sedzinfo/nlp
#   https://rpubs.com/sedzinfo/ocean
##########################################################################################
