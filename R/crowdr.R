#' @importFrom dplyr "%>%" mutate bind_rows
#' @importFrom tibble as_tibble
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom xml2 read_html
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to crowdr, the fastest way to scrape G2 Crowd reviews.")
}


#' Scrape URLs
#'
#' Scrape tool names, URLs, and ratings meta data from a software category on
#' 'G2Crowd.com'.
#'
#' @param category String. Slug of a software category
#' @param pages Integer. Optional. Number of pages that should be scraped.
#'              By default \code{pages} will scrape all available pages.
#' @return Returns a `tibble` with name, URL, number of ratings, and average
#'         rating for each tool of a software category.
#' @export
#' @examples
#' x <- scrape_urls("crm", 3)
scrape_urls <- function(category, pages){

  x <- read_html(paste0("https://www.g2crowd.com/categories/", category))

  # Get number of reviews
  n <- x %>%
    html_nodes("[class='line-height--select bottom-right pr-half']") %>%
    html_nodes("strong") %>%
    html_text() %>%
    as.integer()

  if(length(n) == 0){
    pages <- 1
  } else {
    pages <- ceiling(n/50)
  }



  for(i in 1:pages){
    x <- read_html(paste0("https://www.g2crowd.com/categories/", category,
                          "?order=g2_score&page=", i, "#product-list"))

    if(i == 1){
      urls <- x %>%
        html_nodes("[itemprop=url]") %>%
        html_attr("href") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(value = paste0("https://www.g2crowd.com", value))

      y <- x %>%
        html_nodes("[class='d-f ai-c jc-sb fw-w']") %>%
        html_nodes("a") %>%
        html_text()

      names <- y %>% .[seq(1, length(.), 2)]

      reviews <- y %>%
        .[seq(0, length(.), 2)]  %>%
        gsub("[^[:digit:]\\.[:digit:] |[:digit:] ]", "", .) %>%
        trimws(.) %>%
        strsplit(., " ")

      urls <- urls %>%
        dplyr::bind_cols(names) %>%
        dplyr::bind_cols(reviews)

    } else {
      urls <- x %>%
        html_nodes("[itemprop=url]") %>%
        html_attr("href") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(name = gsub("\\/products\\/|\\/reviews", "", value),
                      value = paste0("https://www.g2crowd.com", value)) %>%
        dplyr::bind_rows(urls)
    }
  }
  return(urls)
}



#' Scrape review texts from urls
#'
#' @description Summarizes merging path by giving pairs of factor groups merged
#' in each iteration.
#' @export
#'
#' @param factorMerger Object of a class \code{factorMerger}
#' @param showStats If \code{TRUE} extends results with
#' the loglikelihood (column \code{model}),
#' p-value for the \code{LRT} tests against the full model (column \code{pval})
#' and Generalized Information Criterion value (column \code{GIC}).
#' By default \code{showStats} is set to \code{FALSE}.
#' @param round Logical. If \code{TRUE}, the default, statistics are rounded
#' @param penalty GIC penalty
#'
#' @examples
#' randSample <- generateMultivariateSample(N = 100, k = 10, d = 3)
#' fm <- mergeFactors(randSample$response, randSample$factor)
#' mergingHistory(fm, showStats = TRUE)

scrape_reviews <- function(urls){

  for(i in 1:nrow(urls)){
    x <- read_html(paste0(urls[i, 1]))

    # Get number of reviews
    n <- x %>%
      html_nodes("[class=filter-summary]") %>%
      html_nodes("strong") %>%
      html_text() %>%
      as.numeric()

    if(length(n) == 0){
      pages <- 1
    } else {
      pages <- ceiling(n/50)
    }
    for(j in 1:pages){
      if(j == 1){
        ratings <- x %>%
          html_nodes("[class='font-weight-strong text-small']") %>%
          html_text() %>%
          tibble::as_tibble()

        titles <- x %>%
          html_nodes("[class='font-weight-bold m-0 text-small mb-4th']") %>%
          html_text() %>%
          tibble::as_tibble()

        #      result <- dplyr::bind_cols(ratings, titles)

      } else {

        y <- read_html(paste0(urls[i, 1], "?page=", j))

        ratings <- y %>%
          html_nodes("[class='font-weight-strong text-small']") %>%
          html_text() %>%
          tibble::as_tibble() %>%
          dplyr::mutate(url = gsub(".+\\/products\\/|\\/reviews", "", urls[i, 1])) %>%
          dplyr::bind_rows(ratings)

        titles <- y %>%
          html_nodes("[class='font-weight-bold m-0 text-small mb-4th']") %>%
          html_text() %>%
          tibble::as_tibble() %>%
          dplyr::bind_rows(titles)

        #      dplyr::bind_rows(result, dplyr::bind_cols(ratings, titles))
      }
    }
  }

  result <- dplyr::bind_cols(ratings, titles) %>%
    dplyr::select(2, 3, 1)
  colnames(result) <- c("Company", "Question", "Answer")

  #  return(ratings)
  return(result)
}