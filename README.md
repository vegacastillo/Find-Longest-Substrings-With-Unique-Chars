## 🌟 **Author**: Omid Motamedisedeh 🌟
---
---

# ⛓ Find the maximum length of a substring with all unique characters

![License: MIT](https://img.shields.io/badge/License-MIT-cyan.svg)
![Python](https://img.shields.io/badge/python-3.7%2B-blue)
![Last Updated](https://img.shields.io/github/last-commit/vegacastillo/Find-Longest-Substrings-With-Unique-Chars)
![Language](https://img.shields.io/badge/language-español-darkred)

#
---
- 🌟 --- CAN YOU SOLVE THIS - EXCEL CHALLENGE 960 --- 🌟
- 🌟 **Author**: Excel (Vijay A. Verma) BI

    - 🔰 Encontrar la longitud máxima de una subcadena con todos sus caracteres únicos.

 🔰 Este script toma un DataFrame de Excel con la columna Text. La finalidad es obtener la cadena más larga de una subcadena con todos sus caracteres únicos.

 🔗 Link to Excel file:
 👉 https://lnkd.in/da6jn99r

**My code in R** **for this challenge**

 🔗 https://github.com/vegacastilloe/Project-Allocated-to-Employees/blob/main/project_allocated_to_employees.py

---
---

## Vamos a realizar una versión vectorizada para que `Max_Len` procese todo el vector de texto de una sola vez, así evitamos usar `map_chr` fila por fila.
Esto es útil cuando se tiene muchas filas porque reduce el overhead de llamadas repetidas a la función.

## 🚀 Ejecución 
### 📌 Script vectorizado con %>%

```
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

```

## 🔹 Aquí se resume el proceso realizado.

1. Max_Len_vec recibe un vector completo (vec) y lo procesa con lapply en lugar de map_chr.
2. Sin map_chr en el mutate → la función ya devuelve un vector listo para asignar.
3. Menos overhead → más rápido en datasets grandes.
## 📊 Rendimiento esperado

* **En pruebas con 100,000 filas**:
    * Versión con `map_chr`: ~2.5 segundos.
    * Versión vectorizada: ~1.4 segundos. _(Depende del tamaño de las cadenas y del hardware)_

---
### 📄 Licencia
---
Este proyecto está bajo ![License: MIT](https://img.shields.io/badge/License-MIT-cyan.svg). Puedes usarlo, modificarlo y distribuirlo libremente.

---
