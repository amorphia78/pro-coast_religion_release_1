# ══════════════════════════════════════════════════════════════════════════════
# Table output shared by the religion analysis scripts
#
# Sourced by src/religion_analysis.R and src/religion_descriptives.R, so the two
# write their tables identically.
#
# Tables are written as Excel workbooks rather than as text files. A TSV has to
# go through Excel's import step, which guesses at types and encodings; a
# workbook opens as-is, with cluster headers merged across the columns they
# span, numbers stored as numbers, and any explanatory note underneath.
# ══════════════════════════════════════════════════════════════════════════════

library(openxlsx)

# A string counts as a number if it is an optionally negative decimal: "0.123",
# "-4", "569". Composite strings such as "0.02 [-0.10, 0.12]" or "1-5" do not.
NUMBER_PATTERN <- "^-?[0-9]+(\\.[0-9]+)?$"

decimals_in <- function(x) {
  ifelse(grepl("\\.", x), nchar(sub("^[^.]*\\.", "", x)), 0L)
}

# Write one table to its own workbook.
#
#   body     a data frame; its column names become the header row, and they
#            may repeat (e.g. two columns both called "Model")
#   path     the .xlsx file to write
#   clusters optional list of list(label, from, to): a header row above the
#            column names, each label merged across columns `from`..`to`
#   note     optional character vector, written one line per row beneath the
#            table after a blank row
#
# Cells are written one at a time so that a number-looking string becomes a
# numeric cell displaying the same decimal places it had as text ("0.230" keeps
# its trailing zero), while any other string stays text. This matters for
# mixed columns such as the correlation table, where r, n and "—" share one.
write_table_xlsx <- function(body, path, clusters = NULL, note = NULL) {
  # By default the file properties record the operating system username.
  wb <- createWorkbook(creator = "")
  addWorksheet(wb, "Table")

  header_style  <- createStyle(textDecoration = "bold", border = "bottom",
                               halign = "center", valign = "center",
                               wrapText = TRUE)
  cluster_style <- createStyle(textDecoration = "bold", halign = "center")
  note_style    <- createStyle(textDecoration = "italic", fontColour = "#404040")
  number_styles <- list()   # one per number of decimal places, made on demand

  row <- 1

  if (!is.null(clusters)) {
    for (cl in clusters) {
      writeData(wb, "Table", cl$label, startCol = cl$from, startRow = row)
      mergeCells(wb, "Table", cols = cl$from:cl$to, rows = row)
      addStyle(wb, "Table", cluster_style, rows = row, cols = cl$from:cl$to,
               gridExpand = TRUE)
    }
    row <- row + 1
  }

  header_row <- row
  for (j in seq_along(body)) {
    writeData(wb, "Table", names(body)[j], startCol = j, startRow = header_row)
  }
  addStyle(wb, "Table", header_style, rows = header_row, cols = seq_along(body),
           gridExpand = TRUE)

  cells <- as.data.frame(lapply(body, as.character), stringsAsFactors = FALSE)
  cells[is.na(cells)] <- ""

  for (i in seq_len(nrow(cells))) {
    r <- header_row + i
    for (j in seq_along(cells)) {
      value <- cells[i, j]
      if (!nzchar(value)) next

      if (grepl(NUMBER_PATTERN, value)) {
        writeData(wb, "Table", as.numeric(value), startCol = j, startRow = r)
        d   <- as.character(decimals_in(value))
        if (is.null(number_styles[[d]])) {
          format <- if (d == "0") "0" else paste0("0.", strrep("0", as.integer(d)))
          number_styles[[d]] <- createStyle(numFmt = format)
        }
        addStyle(wb, "Table", number_styles[[d]], rows = r, cols = j)
      } else {
        writeData(wb, "Table", value, startCol = j, startRow = r)
      }
    }
  }

  last_row <- header_row + nrow(cells)

  if (!is.null(note)) {
    note_rows <- last_row + 1 + seq_along(note)
    for (k in seq_along(note)) {
      writeData(wb, "Table", note[k], startCol = 1, startRow = note_rows[k])
    }
    addStyle(wb, "Table", note_style, rows = note_rows, cols = 1)
  }

  # Widths from the header and body only; the note is long prose and would
  # otherwise stretch the first column across the screen.
  widths <- vapply(seq_along(cells), function(j) {
    longest <- max(nchar(c(names(body)[j], cells[[j]])), 0)
    min(max(longest + 2, 8), 55)
  }, numeric(1))
  setColWidths(wb, "Table", cols = seq_along(cells), widths = widths)

  saveWorkbook(wb, path, overwrite = TRUE)
  drop_orphan_parts(path)
  message("Wrote ", path)
}

# openxlsx 4.2.9 declares a drawing part in every worksheet's relationships, and
# in [Content_Types].xml, without writing the part itself. Strict readers such as
# Python's openpyxl refuse such a file outright, and a dangling reference is the
# kind of thing that can make Excel offer to "repair" a workbook. This rewrites
# the saved archive with every reference to a missing part removed.
drop_orphan_parts <- function(path) {
  # Absolute, because zip::zip() switches into `root` before writing the archive,
  # where a relative path would point somewhere that does not exist.
  path     <- normalizePath(path)
  unpacked <- tempfile("xlsx_")
  on.exit(unlink(unpacked, recursive = TRUE))
  zip::unzip(path, exdir = unpacked)

  part_exists <- function(relative_to, target) {
    file.exists(normalizePath(file.path(unpacked, relative_to, target), mustWork = FALSE))
  }

  # Worksheet relationships: targets are relative to xl/worksheets/.
  for (rels in list.files(file.path(unpacked, "xl", "worksheets", "_rels"),
                          full.names = TRUE)) {
    xml      <- readChar(rels, file.size(rels), useBytes = TRUE)
    elements <- regmatches(xml, gregexpr("<Relationship [^>]*/>", xml))[[1]]
    targets  <- sub('.*Target="([^"]+)".*', "\\1", elements)
    for (k in which(!part_exists(file.path("xl", "worksheets"), targets))) {
      xml <- sub(elements[k], "", xml, fixed = TRUE)
    }
    writeChar(xml, rels, eos = NULL, useBytes = TRUE)
  }

  # Content types: part names are absolute within the archive.
  types    <- file.path(unpacked, "[Content_Types].xml")
  xml      <- readChar(types, file.size(types), useBytes = TRUE)
  elements <- regmatches(xml, gregexpr("<Override [^>]*/>", xml))[[1]]
  parts    <- sub('.*PartName="/([^"]+)".*', "\\1", elements)
  for (k in which(!part_exists(".", parts))) {
    xml <- sub(elements[k], "", xml, fixed = TRUE)
  }
  writeChar(xml, types, eos = NULL, useBytes = TRUE)

  # [Content_Types].xml goes first, as the package format expects.
  files <- list.files(unpacked, recursive = TRUE, all.files = TRUE)
  files <- c("[Content_Types].xml", setdiff(files, "[Content_Types].xml"))
  unlink(path)
  zip::zip(path, files = files, root = unpacked, include_directories = FALSE)
}

# writeLines() to a path would use the system encoding on Windows, so prose files
# go through a binary connection with the text already forced to UTF-8.
write_utf8 <- function(lines, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  message("Wrote ", path)
}
