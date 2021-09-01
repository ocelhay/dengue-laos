library(DiagrammeR)
library(glue)
library(shiny)

ui <- fluidPage(
  grVizOutput("diagram_algo")
)

server <- function(input, output, session) {
  output$diagram_algo <- renderGrViz({
    
    admit <- 140
    pcr_all <- 100
    pcr_pos <- 10
    pcr_equ <- 20
    pcr_neg <- 30
    pcr_nos <- 25
    pcr_nod <- 120
    
    grViz(
      
      glue("
digraph a_nice_graph {{

# node definitions with substituted label text
node [shape = box]
admit [label = 'Admitted patients with suspicion of dengue \n N = {admit}']
pcr_all [label = 'PCR', style = 'filled', fillcolor = 'blue']
pcr_pos [label = 'Positive \n N = {pcr_pos}', style = 'filled', fillcolor = 'red']
pcr_equ [label = 'Equi \n N = {pcr_equ}', style = 'filled', fillcolor = 'green']
pcr_neg [label = 'Negative \n N = {pcr_neg}', style = 'filled', fillcolor = 'yellow']
pcr_nos [label = 'No Sample \n N = {pcr_nos}', style = 'filled', fillcolor = 'gray']
pcr_nod [label = 'Not Done \n N = {pcr_nod}', style = 'filled', fillcolor = 'white']
c [label = 'NS1 Elisa', style = 'filled', fillcolor = 'blue']

# edge definitions with the node IDs
admit -> {{pcr_all}} 
pcr_all -> {{pcr_pos pcr_equ pcr_neg pcr_nos pcr_nod}}
pcr_equ -> {{c}}
pcr_neg -> {{c}}
pcr_nos -> {{c}}
pcr_nod -> {{c}}
}}
")

)
  })
}

shinyApp(ui, server)