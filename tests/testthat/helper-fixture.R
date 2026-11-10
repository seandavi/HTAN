## The fixture is a real-shaped r7 snapshot cut by
## inst/scripts/build_snapshot (`just fixture`): six seed participants and
## their lineage closure, every table present.
fixture_root <- function() {
    system.file("extdata", "snapshot", package = "HTAN", mustWork = TRUE)
}

## Point HTAN at the fixture with a throwaway cache for one test.
local_fixture_snapshot <- function(env = parent.frame()) {
    cache <- withr::local_tempdir(.local_envir = env)
    withr::local_options(
        HTAN.base_url = fixture_root(), HTAN.cache = cache,
        .local_envir = env
    )
    withr::defer(htan_disconnect(), envir = env)
    invisible(cache)
}

skip_if_no_integration <- function() {
    if (!identical(Sys.getenv("HTAN_INTEGRATION"), "true")) {
        testthat::skip("set HTAN_INTEGRATION=true to run live tests")
    }
}

## Files a participant contributed to, including pooled files.
participant_files <- function(id, con = htan_con()) {
    owned <- htan_file_participants(con) |>
        dplyr::filter(HTAN_Participant_ID == !!id)
    htan_files(con) |>
        dplyr::semi_join(owned, by = "HTAN_Data_File_ID")
}
