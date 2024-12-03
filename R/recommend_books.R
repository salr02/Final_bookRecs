#' Recommend Similar Books
#'
#' Recommends books similar to a given book based on a similarity matrix.
#'
#' @param book_name The name of the book for which recommendations are needed.
#' @param book_features A data frame containing book metadata.
#' @param similarity_matrix A precomputed similarity matrix.
#' @param top_n The number of recommendations to return.
#' @return A data frame with the top N recommended books.
#' @export
recommend_books <- function(book_name, book_features, similarity_matrix, top_n = 5) {
  # Find the index of the requested book
  book_index <- which(rownames(similarity_matrix) == book_name)
  if (length(book_index) == 0) stop("Book not found in the similarity matrix.")

  # Get similarity scores for the target book
  book_similarity <- similarity_matrix[book_index, ]

  # Combine similarity scores with book metadata
  book_features$Similarity <- book_similarity

  # Sort by similarity and return the top N similar books
  recommended_books <- book_features %>%
    arrange(Similarity) %>%
    head(top_n)

  return(recommended_books)
}




