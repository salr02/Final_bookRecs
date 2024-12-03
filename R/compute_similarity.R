#' Compute Similarity Matrix
#'
#' Computes a similarity matrix based on normalized ratings or other features in the book dataset.
#'
#' @param book_features A data frame containing book metadata with normalized features.
#' @param feature_cols A character vector specifying the column names to use for similarity computation.
#' @return A matrix of similarity scores.
#' @export
compute_similarity_matrix <- function(book_features, feature_cols = c("Normalized_Rating")) {
  # Ensure required columns are available
  if (!all(feature_cols %in% colnames(book_features))) {
    stop("Specified feature columns not found in the dataset.")
  }

  # Compute similarity matrix using Euclidean distance
  similarity_matrix <- proxy::dist(book_features[, feature_cols, drop = FALSE], method = "euclidean")

  # Convert to matrix for easier manipulation
  return(as.matrix(similarity_matrix))
}
