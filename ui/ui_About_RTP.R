tabPanel("Redundant Target Paradigm (RTP)", value = "rsp",
    h2("The Redundant Target Paradigm (RTP)"),
    p("In the Redundant Target/ Signals Paradigm, the participant is instructed
      to respond to the first stimulus, regardless of the modality."),
    p("For RTP, multisensory integration occurs only in two events, which are
      united in
      $$I_{RTP} = \\{A + \\tau < V < A + \\tau + \\omega\\} \\cup
                  \\{V < A + \\tau < V + \\omega\\},$$
      where \\(A\\) and \\(V\\) denote the peripheral processing times for the
      acoustic and the visual stimulus, \\(\\omega\\) denotes the width of the
      time window of integration, and \\(\\tau\\) denotes the stimulus onset
      asynchrony (SOA). For SOA, the presentation of the visual stimulus
      arbitrarily defines the zero time point."),
    h4("Probability of Integration"),
    p("The probability of integration \\(P(I_{RTP})\\) is computed as the sum
      of the probabilities of the two events above, \\(P(I_{RTP}) = p_1 +
      p_2\\). In the first case, the", strong("acoustic stimulus wins"), "the
      race in the first stage:
      $$p_1 = P(A + \\tau < V < A + \\tau + \\omega) =
      \\int_0^{\\infty}\\{F_V(a + \\tau + \\omega) - F_V(a + \\tau)\\}
      \\,\\mathrm{d}F_A(a).$$
      In the second case the", strong("visual stimulus wins"), ":
      $$p_2 = P(V < A + \\tau < V + \\omega) =
      \\int_0^{\\infty}\\{F_A(v + \\omega - \\tau) - F_A(v - \\tau)\\}
      \\,\\mathrm{d}F_V(v).$$
      The values of these integrals depend on signs of \\(\\tau\\) and
      \\(\\omega\\) + \\(\\tau\\)."),
    tags$ul(
        tags$li("For \\(\\tau < 0\\) (which means the", strong("acoustic stimulus is
            presented first),"),
            "$$p_1 = \\begin{cases}
                \\frac{\\lambda_V}{\\lambda_V + \\lambda_A}
                    \\left\\{\\exp(\\lambda_A(\\tau + \\omega)) -
                    \\exp(\\lambda_A \\tau)\\right\\}
                    & \\quad \\text{if } \\tau + \\omega < 0,\\\\
                \\frac{\\lambda_A}{\\lambda_V + \\lambda_A} \\left\\{1 -
                    \\exp(-\\lambda_V(\\tau + \\omega))\\right\\} +
                    \\frac{\\lambda_V}{\\lambda_V + \\lambda_A} \\left\\{1 -
                    \\exp(\\lambda_A \\tau)\\right\\}
                    & \\quad \\text{if } \\tau < 0 < \\tau + \\omega,
            \\end{cases}$$
            and
            $$p_2 = \\frac{\\lambda_V}{\\lambda_V + \\lambda_A} \\left\\{
                \\exp(\\lambda_A \\tau) - \\exp(-\\lambda_A(\\omega -
                 \\tau))\\right\\}.$$"),
        tags$li("For \\(\\tau > 0\\) (which means the", strong("visual stimulus is
            presented first),"),
            "$$p_1 = \\frac{\\lambda_A}{\\lambda_V + \\lambda_A} \\left\\{
                \\exp(-\\lambda_V \\tau) - \\exp(-\\lambda_V(\\omega +
                 \\tau))\\right\\},$$
            and
            $$p_2 = \\begin{cases}
                \\frac{\\lambda_A}{\\lambda_V + \\lambda_A}
                    \\left\\{\\exp(\\lambda_V(\\omega - \\tau)) -
                    \\exp(-\\lambda_V \\tau)\\right\\}
                    & \\quad \\text{if } \\tau > \\omega,\\\\
                \\frac{\\lambda_A}{\\lambda_V + \\lambda_A} \\left\\{1 -
                    \\exp(-\\lambda_V \\tau)\\right\\} +
                    \\frac{\\lambda_V}{\\lambda_V + \\lambda_A} \\left\\{1 -
                    \\exp(-\\lambda_A(\\omega - \\tau))\\right\\}
                    & \\quad \\text{if } \\tau < \\omega.
            \\end{cases}$$")
      ))
