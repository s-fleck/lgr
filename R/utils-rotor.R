assert_valid_compression <- function(compression){
  assert(
    is_scalar_atomic(compression) && (
      compression %in% c("base::zip", "zip::zipr") ||
        compression %in% 1:9 ||
        is_bool(compression)
    ),
    '`compression` must be `TRUE`, `FALSE`, or an integer between 1 and 9',
    'or the character scalers "base::zip" or "zip::zipr" not: ',
    preview_object(compression)
  )
}
