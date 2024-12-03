
#' @param target_book The name of the target book.
#' @param book_features A data frame containing book metadata.
#' @param similarity_matrix A matrix of similarity scores.
#' @param top_n Number of similar books to visualize.
#' @return A ggplot2 bar plot.
#' @export
visualize_similarities <- function(target_book, book_features, similarity_matrix, top_n = 5) {
  library(ggplot2)

  if (!target_book %in% rownames(similarity_matrix)) {
    stop("Target book not found in the similarity matrix.")
  }

  book_index <- which(rownames(similarity_matrix) == target_book)
  similarity_scores <- similarity_matrix[book_index, ]

  similar_books <- data.frame(
    Book = rownames(similarity_matrix),
    Similarity = similarity_scores
  )
  similar_books <- similar_books[order(similar_books$Similarity), ][1:top_n, ]

  similar_books <- similar_books[similar_books$Book != target_book, ]

  #the bar plot
  ggplot(similar_books, aes(x = reorder(Book, -Similarity), y = Similarity)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = paste("Top Similar Books to", target_book),
      x = "Books",
      y = "Similarity Score"
    ) +
    theme_minimal()
}
