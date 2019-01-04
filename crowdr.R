# Scrape URLs from pages
scrape_urls <- function(category, pages){
  for(i in 1:pages){
    x <- read_html(paste0("https://www.g2crowd.com/categories/", category, 
                          "?order=g2_score&page=", i, "#product-list"))
    
    if(i == 1){
      urls <- x %>% 
        html_nodes("[itemprop=url]") %>%
        html_attr("href") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(value = paste0("https://www.g2crowd.com", value))
      
    } else {
      urls <- x %>% 
        html_nodes("[itemprop=url]") %>%
        html_attr("href") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(value = paste0("https://www.g2crowd.com", value)) %>%
        dplyr::bind_rows(urls)
    }
  }
  return(urls)
}

# Scrape review texts from urls

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
          tibble::as_tibble() %>%
          dplyr::mutate(url = gsub(".+\\/products\\/|\\/reviews", "", urls[i, 1]))
        
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
