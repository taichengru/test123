library(shiny)

ui <- bootstrapPage(
  h3("Parsed query string"),
  verbatimTextOutput("queryText"),
  h3("URL components"),
  verbatimTextOutput("sessionText"),
  h3("EnvVars"),
  verbatimTextOutput("envvarText")
)

server <- function(input, output, session) {
  # Parse the GET query string
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    # Return a string with key-value pairs
    paste(names(query), query, sep = "=", collapse=", ")
  })
  # Return the components of the URL in a string:
  output$sessionText <- renderText({
    cls <- sapply(session, function(a) class(a)[1])
    nms <- names(cls[ cls %in% c("list", "character", "numeric", "integer",
                                 "NULL", "logical", "environment", "reactivevalues" ) ])
    nms <- setdiff(nms, ".__enclos_env__")
    paste(
      capture.output(
        str(
          sapply(nms,
                 function(sessnm) {
                   if (inherits(session[[sessnm]], c("environment", "reactivevalues"))) {
                     sapply(names(session[[sessnm]]), function(nm) session[[sessnm]][[nm]], simplify = FALSE)
                   } else if (inherits(session[[sessnm]], c("character", "numeric", "integer"))) {
                     session[[sessnm]]
                   } else class(session[[sessnm]])
                 }, simplify = FALSE),
          nchar.max = 1e5,
          vec.len = 1e5
        )
      ),
      collapse = "\n"
    )
  })
  # Dump the environment variables
  output$envvarText <- renderText({
    paste(
      capture.output(
        str(as.list(Sys.getenv()))
      ),
      collapse = "\n"
    )
  })
}

shinyApp(ui, server)