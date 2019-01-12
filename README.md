# crowdr
Scrape g2crowd reviews with R

# Usage

## Scrape urls from category pages

    scrape_urls(category)

Example usage:

    > scrape_urls("crm")
    # A tibble: 330 x 4
       name                            no_reviews avg_rating url                                              
       <chr>                                <dbl>      <dbl> <chr>                                            
     1 Salesforce CRM                        7177        4.2 https://www.g2crow…
     2 HubSpot CRM                           2241        4.2 https://www.g2crow…
     3 Microsoft Dynamics 365 for Sal…        868        3.6 https://www.g2crow…
     4 Freshsales                             444        4.7 https://www.g2crow…
     5 Pipedrive                              787        4.3 https://www.g2crow…
     6 Zoho CRM                               890        3.8 https://www.g2crow…
     7 Zendesk Sell                           323        4.2 https://www.g2crow…
     8 Copper                                 388        4.6 https://www.g2crow…  
     9 PipelineDeals                          525        4.4 https://www.g2crow…
    10 Nimble                                 550        4.5 https://www.g2crow…  
    # … with 320 more rows
    
    

## Scrape review texts from urls (not functional, yet)
    scrape_reviews(urls)


# Changelog 

## 2019-01-12 v0.2.0

Updates

* scrape_urls() works as intended

Known Bugs

* Output data type is "factor"
