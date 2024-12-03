
#'
#' @return Saves the similarity matrix to the `data/` directory.
precompute_similarity <- function() {
  # Load the dataset
  data("book1_100k", package = "sandersonRatings")

  # Compute similarity matrix
  library(proxy)
  similarity_matrix <- dist(book1_100k$Normalized_Rating, method = "euclidean")
  similarity_matrix <- as.matrix(similarity_matrix)

  # Save to the data directory
  save(similarity_matrix, file = "~/sandersonRatings/data/similarity_matrix.rda")
}
