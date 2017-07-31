tabPanel("Redundant Target Paradigm (RTP)", value = "rsp",
    h2("The Redundant Target Paradigm (RTP)"),
    withMathJax(p("In the Redundant Target/ Signals Paradigm, the participant
                  is instructed to respond to the first stimulus, regardless of
                  the modality."),
    p("For RTP, multisensory integration occurs only in two events, which are
      united in
      $$I_{RTP} = \\{A + \\tau < V < A + \\tau + \\omega\\} \\cup
                  \\{V < A + \\tau < V + \\omega\\},$$
    where \\(A\\) and \\(V\\) denote the peripheral processing times for the
    acoustic and the visual stimulus, \\(\\omega\\) denotes the width of the time
    window of integration, and \\(\\tau\\) denotes the stimulus onset asynchrony
    (SOA). For SOA, the presentation of the visual stimulus arbitrarily defines
    the zero time point."),
    h4("Probability of Integration"),
    p("The probability of integration \\(P(I_{RTP})\\) is computed as the sum of
      the probabilities of the two events above. In the first case, the",
      strong("acoustic stimulus wins"), "the race in the first stage:
      $$P(A + \\tau < V < A + \\tau + \\omega) =
      \\int_0^{\\infty}\\{F_V(a + \\tau + \\omega) - F_V(a + \\tau)\\}
      \\mathrm{d}F_A(a).$$
      In the second case the", strong("visual stimulus wins"), ":
      $$P(V < A + \\tau < V + \\omega) =
      \\int_0^{\\infty}\\{F_A(v + \\omega - \\tau) - F_A(v - \\tau)\\}
      \\mathrm{d}F_V(v).$$
      The values of these integrals depend on signs of \\(\\tau\\) and
      \\(\\omega\\) + \\(\\tau\\)."),
    p("For \\(\\tau < 0\\), which means the", strong("acoustic stimulus is
      presented first")),
        tags$ul(tags$li(
              p("The Probalility of Integration for cases, in which the",
                strong("visual"), "stimulus is presented first (e.g., SOAs of
                0, 50 and 100 ms) depending on the
                signs of \\(\\omega\\) + \\(\\tau\\):"),
              p("(i)", align = "center"),
              p("$$\\begin{align}
                P(I) = 
                \\end{align}$$"),
              p("(ii)", align = "center"),
              p("$$\\begin{align}
                P(I) = 
                \\end{align}$$"),
              p("(iii)", align = "center"),
              p("$$\\begin{align}
                P(I) = 
                \\end{align}$$")),
        tags$li(   
              p("The Probalility of Integration for cases, in which the",
                strong("auditory"), "stimulus is presented first (e.g., SOAs of
                -100, -50 and 0 ms):"),
              p("(i) ", align = "center"),
              p("$$\\begin{align}
                P(I) = 
                \\end{align}$$"),
              p("(ii)", align = "center"),
              p("$$\\begin{align}
                P(I) = 
                \\end{align}$$"),
              p("(iii)", align = "center"),
              p("$$\\begin{align}
                P(I) = 
                \\end{align}$$")))
      ))
