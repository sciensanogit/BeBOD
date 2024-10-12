rmarkdown_system_file <-
function (file) {
  system.file(file, package = "rmarkdown")
}

html_body <-
  function() {
    base <-
      html_document(
        number_sections = FALSE,
        fig_width = 7,
        fig_height = 5,
        fig_retina = 2,
        fig_caption = FALSE,
        dev = 'png',
        smart = TRUE,
        keep_md = FALSE,
        md_extensions = NULL,
        pandoc_args = "--mathjax",
        mathjax = NULL,
        highlight = "default",
        theme = NULL,
        template = rmarkdown_system_file("rmd/fragment/default.html")
      )
    
    ## remove divs
    ## .. needed for h2-based toc
    base$pandoc$args <- setdiff(base$pandoc$args, "--section-divs")

    return(base)
  }