#' @importFrom dplyr "%>%"
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom xml2 read_html
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to crowdr, the fastest way to scrape G2 Crowd reviews.")
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## Thanks Jenny Bryan
if(getRversion() >= "2.3.0")  utils::globalVariables(c("."))



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
#' x <- scrape_urls("crm")
#' x <- scrape_urls("crm", 3L)
scrape_urls <- function(category, pages = NULL){

  x <- read_html(paste0("https://www.g2crowd.com/categories/", category))

  # Get number of reviews
  n <- x %>%
    html_nodes("[class='line-height--select bottom-right pr-half']") %>%
    html_nodes("strong") %>%
    html_text() %>%
    as.integer()

  maxpages <- ceiling(n/50)

  if (is.null(pages)) {
    pages <- maxpages
  } else if (!is.integer(pages)) {
    stop("The pages variable has to be NULL or an integer.")
  } else if (pages > maxpages) {
    stop(paste("Category", category, "only has", maxpages, "pages. Cannot scrape more than that."))
  } else if (length(maxpages) == 0) {
    stop("Error while trying to determine number of possible pages. (Result was NULL)")
  }


  for(i in 1:pages){
    x <- read_html(paste0("https://www.g2crowd.com/categories/", category,
                          "?order=g2_score&page=", i, "#product-list"))

    if(i == 1){
      urls <- x %>%
        html_nodes("[itemprop=url]") %>%
        html_attr("href") %>%
        tibble::enframe(name = NULL) %>%
        dplyr::mutate(value = paste0("https://www.g2crowd.com", .$value))

      y <- x %>%
        html_nodes("[class='d-f ai-c jc-sb fw-w']") %>%
        html_nodes("a") %>%
        html_text()

      names <- y %>% .[seq(1, length(.), 2)]

      reviews <- y %>%
        .[seq(0, length(.), 2)]  %>%
        gsub("[^[:digit:]\\.[:digit:] |[:digit:] ]", "", .) %>%
        trimws(.) %>%
        unlist(strsplit(., " ")) %>%
        tibble::enframe(name = NULL)

      reviews <- lapply(strsplit(reviews$value, " "), rev)
      reviews <- as.data.frame(do.call(rbind, reviews), stringsAsFactors = FALSE)

      # Fixes a bug where strsplit does not work on a page without reviews
      if (dim(reviews)[2] == 1) {
        reviews[, 2] <- "0"
      }

      result <- data.frame(name = names,
                           no_reviews = reviews[, 1],
                           avg_rating = reviews[, 2],
                           # Assigning a name does not work
                           urls[, 1],
                           stringsAsFactors = FALSE)

    } else {
      urls <- x %>%
        html_nodes("[itemprop=url]") %>%
        html_attr("href") %>%
        tibble::enframe(name = NULL) %>%
        dplyr::mutate(value = paste0("https://www.g2crowd.com", .$value))

      y <- x %>%
        html_nodes("[class='d-f ai-c jc-sb fw-w']") %>%
        html_nodes("a") %>%
        html_text()

      names <- y %>% .[seq(1, length(.), 2)]

      reviews <- y %>%
        .[seq(0, length(.), 2)]  %>%
        gsub("[^[:digit:]\\.[:digit:] |[:digit:] ]", "", .) %>%
        trimws(.) %>%
        unlist(strsplit(., " ")) %>%
        tibble::enframe(name = NULL)

      # Test
      reviews <- lapply(strsplit(reviews$value, " "), rev)
      reviews <- as.data.frame(do.call(rbind, reviews))

      if (dim(reviews)[2] == 1) {
        reviews$V2 <- 0
      }

      result <- rbind(result, data.frame(name = names,
                                         no_reviews = reviews[, 1],
                                         avg_rating = reviews[, 2],
                                         # Assigning a name does not work
                                         urls[, 1],
                                         stringsAsFactors = FALSE))

    }


  }
  # Correct data type
  result[, 2] <- as.numeric(result[, 2])
  result[, 3] <- as.numeric(result[, 3])

  result <- tibble::as_tibble(result)

  colnames(result) <- c("name", "no_reviews", "avg_rating", "url")
  return(result)
}



#' Scrape review texts from urls
#'
#' @description Summarizes merging path by giving pairs of factor groups merged
#' in each iteration.
#' @export
#'
#' @param urls Vector of URLs
#'
#' @examples
#' x <- scrape_revies(urls)

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
