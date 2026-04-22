library(tidyverse)
library(readxl)

# Parámetros
File_name <- "C:\\Excel_Challenge_960 - Longest Substrings With Unique Chars.xlsx"

# Validación de archivo
if (!file.exists(File_name)) {
  stop("❌ El archivo especificado no existe: ", File_name)
}

# Función vectorizada para obtener la cadena más larga sin duplicados
Max_Len_vec <- function(vec) {
  # Procesar todas las cadenas en una sola operación
  results <- lapply(vec, function(x) {
    str_split_1(x, "") %>%
      accumulate(paste0, .dir = "backward") %>%
      map(~ {
        tibble(Values = str_split_1(.x, "")) %>%
          mutate(IsDup = !duplicated(Values),
                 Break = cumall(IsDup)) %>%
          filter(Break) %>%
          reframe(Result = paste(Values, collapse = ""),
                  N = nchar(Result))
      }) %>%
      bind_rows() %>%
      slice_max(N, with_ties = TRUE) %>%
      distinct(Result) %>%
      reframe(Result = paste(Result, collapse = ",")) %>%
      pull(Result)
  })
  
  # Convertir la lista a un vector de caracteres
  unlist(results, use.names = FALSE)
}

# Lectura de columnas A y B en una sola llamada
df <- read_xlsx(File_name, range = "A1:B15", col_names = TRUE, sheet = "Sheet1") %>%
  rename(Text = 1, Answer_Expected = 2) %>%
  mutate(My_Answer = Max_Len_vec(Text))  # Procesar todo el vector

# Mostrar resultado (opcional)
print(df)
# Comparar el resultado
all.equal(df$Answer_Expected, df$My_Answer)
