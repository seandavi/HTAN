#!/usr/bin/env Rscript
## Format the R chunks of Quarto files with Air, which formats .R files
## only. Each ```{r} chunk body goes through `air format --stdin-file-path`
## so it picks up air.toml. Usage:
##   Rscript dev/air-qmd.R [--check] file.qmd ...
## With --check, nothing is written and the exit status is 1 if any chunk
## would change. Used by `just format`, `just format-check` and the
## pre-commit hook.

args <- commandArgs(trailingOnly = TRUE)
check <- "--check" %in% args
files <- setdiff(args, "--check")

format_chunk <- function(code, path) {
    out <- suppressWarnings(system2(
        "air",
        c("format", "--stdin-file-path", shQuote(path)),
        input = code,
        stdout = TRUE,
        stderr = TRUE
    ))
    status <- attr(out, "status")
    if (!is.null(status) && status != 0L) {
        stop(
            "air failed on a chunk of ",
            path,
            ":\n",
            paste(out, collapse = "\n"),
            call. = FALSE
        )
    }
    out
}

changed <- character()
for (f in files) {
    lines <- readLines(f, warn = FALSE)
    open <- grep("^```\\{r[ ,}]", lines)
    close <- grep("^```[[:space:]]*$", lines)
    out <- lines
    ## Work bottom-up so earlier line numbers stay valid.
    for (s in rev(open)) {
        e <- min(close[close > s])
        if (e == s + 1L) {
            next
        }
        body <- lines[(s + 1L):(e - 1L)]
        new <- format_chunk(body, sub("\\.qmd$", ".R", f))
        if (!identical(new, body)) {
            out <- c(out[seq_len(s)], new, out[e:length(out)])
        }
    }
    if (!identical(out, lines)) {
        changed <- c(changed, f)
        if (!check) writeLines(out, f)
    }
}
if (length(changed)) {
    message(
        if (check) "Would reformat: " else "Reformatted: ",
        paste(changed, collapse = ", ")
    )
    if (check) quit(status = 1L)
}
