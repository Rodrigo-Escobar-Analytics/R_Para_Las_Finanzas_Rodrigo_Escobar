

tq_get_batch <- function(
    TICKERS,
    from,
    batch_size = 30,
    sleep_sec = 5
) {
  
  batches <- split(
    TICKERS,
    ceiling(seq_along(TICKERS) / batch_size)
  )
  
  map_dfr(batches, function(batch) {
    
    Sys.sleep(sleep_sec)
    
    tryCatch(
      {
        tq_get(
          batch,
          get  = "stock.prices",
          from = from
        )
      },
      error = function(e) {
        message("Batch failed: ", paste(batch, collapse = ", "))
        NULL
      }
    )
    
  })
}
