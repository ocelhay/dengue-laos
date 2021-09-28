output$diagram_algo <- renderGrViz({
  
  admit <- dengue_dta_filt() %>% nrow()
  pcr_pos <- dengue_dta_filt() %>% filter(pcr_result == "Positive") %>% nrow()
  pcr_equ <- dengue_dta_filt() %>% filter(pcr_result == "Equivocal") %>% nrow()
  pcr_neg <- dengue_dta_filt() %>% filter(pcr_result == "Negative") %>% nrow()
  pcr_nos <- dengue_dta_filt() %>% filter(pcr_result == "Unknown") %>% nrow()
  pcr_nod <- dengue_dta_filt() %>% filter(pcr_result == "not done") %>% nrow()
  
  ns1_pos <- dengue_dta_filt() %>% filter(elisa_ns1_test_result == "Positive") %>% nrow()
  ns1_equ <- dengue_dta_filt() %>% filter(elisa_ns1_test_result == "Equivocal") %>% nrow()
  ns1_neg <- dengue_dta_filt() %>% filter(elisa_ns1_test_result == "Negative") %>% nrow()
  ns1_nos <- dengue_dta_filt() %>% filter(elisa_ns1_test_result == "Unknown") %>% nrow()
  ns1_nod <- dengue_dta_filt() %>% filter(elisa_ns1_test_result %in% c("Not done", "not done (PCR+)")) %>% nrow()
  
  igm_pos <- dengue_dta_filt() %>% filter(elisa_ig_m_test_result %in% c("Positive", "positive")) %>% nrow()
  igm_equ <- dengue_dta_filt() %>% filter(elisa_ig_m_test_result == "Equivocal") %>% nrow()
  igm_neg <- dengue_dta_filt() %>% filter(elisa_ig_m_test_result == "Negative") %>% nrow()
  igm_nos <- dengue_dta_filt() %>% filter(elisa_ig_m_test_result == "Unknown") %>% nrow()
  igm_nod <- dengue_dta_filt() %>% filter(elisa_ig_m_test_result %in% c("Not done", "not done (PCR+)", "not done (NS1+)")) %>% nrow()

  grViz(
    glue("
digraph a_nice_graph {{

# node definitions with substituted label text
node [shape = box]
admit [label = 'Admitted patients with suspicion of dengue \n N = {admit}']
pcr_all [label = 'PCR']
pcr_pos [label = 'Positive \n N = {pcr_pos}', style = 'filled', fillcolor = '#e9a3c9']
pcr_equ [label = 'Equivocal \n N = {pcr_equ}', style = 'filled', fillcolor = '#a1d76a']
pcr_neg [label = 'Negative \n N = {pcr_neg}', style = 'filled', fillcolor = '#d9d9d9']
pcr_nos [label = 'No Sample \n N = {pcr_nos}', style = 'filled', fillcolor = 'white']
pcr_nod [label = 'Not Done \n N = {pcr_nod}', style = 'filled', fillcolor = 'white']
ns1_all [label = 'NS1 Elisa']
ns1_pos [label = 'Positive \n N = {ns1_pos}', style = 'filled', fillcolor = '#e9a3c9']
ns1_equ [label = 'Equivocal \n N = {ns1_equ}', style = 'filled', fillcolor = '#a1d76a']
ns1_neg [label = 'Negative \n N = {ns1_neg}', style = 'filled', fillcolor = '#d9d9d9']
ns1_nos [label = 'No Sample \n N = {ns1_nos}', style = 'filled', fillcolor = 'white']
ns1_nod [label = 'Not Done \n N = {ns1_nod}', style = 'filled', fillcolor = 'white']
igm_all [label = 'IgM Elisa']
igm_pos [label = 'Positive \n N = {igm_pos}', style = 'filled', fillcolor = '#e9a3c9']
igm_equ [label = 'Equivocal \n N = {igm_equ}', style = 'filled', fillcolor = '#a1d76a']
igm_neg [label = 'Negative \n N = {igm_neg}', style = 'filled', fillcolor = '#d9d9d9']
igm_nos [label = 'No Sample \n N = {igm_nos}', style = 'filled', fillcolor = 'white']
igm_nod [label = 'Not Done \n N = {igm_nod}', style = 'filled', fillcolor = 'white']

# edge definitions with the node IDs
admit -> {{pcr_all}} 
pcr_all -> {{pcr_pos pcr_equ pcr_neg pcr_nos pcr_nod}}
pcr_equ -> {{ns1_all}}
pcr_neg -> {{ns1_all}}
pcr_nos -> {{ns1_all}}
pcr_nod -> {{ns1_all}}
ns1_all -> {{ns1_pos ns1_equ ns1_neg ns1_nos ns1_nod}}
ns1_equ -> {{igm_all}}
ns1_neg -> {{igm_all}}
ns1_nos -> {{igm_all}}
ns1_nod -> {{igm_all}}
igm_all -> {{igm_pos igm_equ igm_neg igm_nos igm_nod}}
}}
")

  )
})