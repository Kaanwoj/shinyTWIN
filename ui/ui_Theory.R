tabPanel("Theory", value = "Theory",
fluidRow(
  column(11,
  navlistPanel(
    "Setting",
    tabPanel("Paradigms",
      h4("Paradigms"),
      h5("Experimental Setup"),
        withTags({
          div(class="panel-group",
            div(class="panel panel-default",
              div(class="panel-heading", "Crossmodal condition:"),
              div(class="panel-body",
                "Two (or more) stimuli are presented in different modalities
                simultaneously or with a short delay (stimulus-onset
                asynchrony) between the stimuli.")),
            div(class="panel panel-default",
              div(class="panel-heading", "Unimodal condition:"),
              div(class="panel-body",
                "One stimulus is presented in one modality."))
        )}),
      h5("Participant's Task"),
      p("... depends on the paradigm."),
      fluidRow(
        column(6,
          withTags({
            div(class="panel panel-default",
              div(class="panel-heading", "Focused Attention Paradigm (FAP)"),
              div(class="panel-body",
                p(i("Respond as quickly as possible to a stimulus of a
                  pre-defined modality (target) and ignore the other stimulus
                  (non-target modality).")),
                p("In the following we assume that the visual stimulus is the
                  target modality, and the auditory stimulus is the non-target
                  modality.")))
        })),
        column(6,
          withTags({
            div(class="panel panel-default",
              div(class="panel-heading", "Redundant Target Paradigm (RTP)"),
              div(class="panel-body",
                i("Respond to a stimulus of any modality detected first.")))
        }))
    )),
    "Assumptions",
    tabPanel("Hypothesis",
      h4("Hypothesis"),
      HTML("Multisensory integration of sensory information into a
        perceptual unit (<em>event</em>) occurs only if the peripheral sensory
        processes all terminate within a given temporal interval, the
        <B>Time-Window of Integration</B>."), br(),
      img(src="TWIN.png", width = "50%"), br(),
      withTags({
        # Panel Collapse Accordion
        div(class="panel-group", id="accordionAssumptions",
        # First Stage Assumption
          div(class="panel panel-default",
            div(class="panel-heading",
              h5(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordionAssumptions",
                  href="#collapse1", "First Stage Assumption"))),
            div(id="collapse1", class="panel-collapse collapse in",
              div(class="panel-body",
                "The first stage consists in a (stochastically independent)
                race among the peripheral processes in the visual and auditory
                pathways triggered by a crossmodal stimulus complex."))),
        # Second Stage Assumption
          div(class="panel panel-default",
            div(class="panel-heading",
              h5(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordionAssumptions",
                  href="#collapse2", "Second Stage Assumption"))),
            div(id="collapse2", class="panel-collapse collapse",
              div(class="panel-body",
                "The second stage comprises all processes following the first
                stage including preparation and execution of a response."))),
        # Time-Window of Integration Assumption"
          div(class="panel panel-default",
            div(class="panel-heading",
              h5(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordionAssumptions",
                  href="#collapse3", "Time-Window of Integration Assumption"))),
            div(id="collapse3", class="panel-collapse collapse",
              div(class="panel-body",
                "Multisensory integration occurs only if the peripheral
                processes of the first stage all terminate within a given
                temporal interval, the time window of integration."))),
        # Temporal Separability Assumption
          div(class="panel panel-default",
            div(class="panel-heading",
              h5(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordionAssumptions",
                  href="#collapse4", "Assumption of Temporal Separability"))),
            div(id="collapse4", class="panel-collapse collapse",
              div(class="panel-body",
                "The amount of interaction, manifesting itself in an increase
                or decrease of second stage processing time, is a function of
                crossmodal stimulus features, but it does not depend on the
                presentation asynchrony (SOA) of the stimuli.")))
    )})),
    tabPanel("Reaction Times",
      h4("Reaction Times"),
      p("The total reaction time in the crossmodal condition (\\(RT_{VA})\\) is
        the sum of the processing times in stage 1 and stage 2. Let \\(M1\\)
        and \\(M2\\) denote two random variables that refer to the first and
        second stage processing times, so that the overall reaction time
        becomes $$RT_{VA} = M_1 + M_2.$$"),
      tags$ul(
        tags$li("\\(I\\) is the event that multisensory integration occurs, with
          probability \\(P(I)\\)."),
        tags$li("\\(E[M_2|\\neg I]\\) is the expected stage 2 processing time
                conditioned on multisensory integration not occuring."),
        tags$li("\\(\\Delta\\) is the size of the crossmodal interaction
                effect.")),
      withTags({
        div(class="panel-group",
          div(class="panel panel-default",
            div(class="panel-heading", "Expected reaction times for the crossmodal
                condition:"),
            div(class="panel-body",
                p("$$E[RT_{VA}] = E[M_1] + E[M_2|\\neg I] - P(I) \\cdot
                                                                \\Delta$$"),
                a(href="#rtcross", class="btn", 'data-toggle'="collapse",
                  "See derivation"),
                div(id="rtcross", class="collapse",
                  p("$$\\begin{align}
                    E[RT_{VA}] &= E[M_1] + E[M_2] \\\\
                                &= E[M_1] + P(I) \\cdot E[M_2|I] + (1-P(I))
                                   \\cdot E[M_2|\\neg I] \\\\
                                &= E[M_1] + E[M_2|\\neg I] - P(I) \\cdot
                                   (E[M_2|\\neg I] - E[M_2|I]) \\\\
                                &= E[M_1] + E[M_2|\\neg I] - P(I) \\cdot \\Delta
                  \\end{align}$$
                  with \\(\\Delta \\equiv E[M_2|\\neg I] - E[M_2|I]\\)")
                )
            )),
          div(class="panel panel-default",
            div(class="panel-heading", "Expected reaction times for the
                unimodal conditions:"),
            div(class="panel-body",
                p("$$E[RT_{V}] = E[V] + E[M_2|\\neg I]$$"),
              p("")))
      )})
    ),
    tabPanel("Random Variable Distributions",
      h4("Random Variable Distributions"),
      h5("First Stage"),
      p("Random variable \\(M_1\\) refers to the peripheral processing time
        in stage 1. For the processing times for the visual and the
        acoustic stimulus, ", strong("V and A denote the two statistically
        independent, exponentially distributed random variables.")),
      a(href="#densityfirst", 'data-toggle'="collapse",
        "See density functions"),
      div(id="densityfirst", class="collapse",
        p("The density functions are then
        $$\\begin{align}
        f_V(t) = \\lambda_{V}e^{-\\lambda_{V}t} \\\\
        f_A(t) = \\lambda_{A}e^{-\\lambda_{A}t}
        \\end{align}$$
        for \\(t \\geq 0\\), and \\(f_V(t) = f_A(t) \\equiv 0\\) for \\(t<0.\\)
        The corresponding distribution functions are
        \\(F_V(t)\\) and \\(F_A(t)\\), respectively.")),
      h5("Second Stage"),
      p("Random variable \\(M_2\\) refers to the processing time in stage two
        and is assumed to be ", strong("normally distributed"), "with mean
        \\(\\mu - \\Delta\\) in the crossmodal condition and mean \\(\\mu\\) in
        the unimodal condition."),
      h5("Expected Reaction Times"),
      withTags({
        div(class="panel-group",
          div(class="panel panel-default",
            div(class="panel-heading", "Expected reaction times for the crossmodal
                condition:"),
            div(class="panel-body",
              fluidRow(
                column(6,
                  withTags({
                    div(class="panel panel-default",
                      div(class="panel-heading", "Focused Attention Paradigm (FAP)"),
                      div(class="panel-body",
                        p("$$ E[RT_{VA}] = \\tfrac{1}{\\lambda_V}
                          + \\mu - P(I_{FAP}) \\cdot \\Delta$$")
                        ))
                })),
                column(6,
                  withTags({
                    div(class="panel panel-default",
                      div(class="panel-heading", "Redundant Target Paradigm (RTP)"),
                      div(class="panel-body",
                        p("$$E[RT_{VA}] = E[min(V,A)] + \\mu - P(I_{RTP})
                          \\cdot \\Delta$$")
                        ))
                }))
              )
            )),
          div(class="panel panel-default",
            div(class="panel-heading", "Expected reaction times for the
                unimodal condition:"),
            div(class="panel-body",
              p("$$\\begin{align} E[RT_V] = \\tfrac{1}{\\lambda_V} + \\mu
                \\end{align}$$")))
      )})
    ),
    tabPanel("Probability of Integration",
      h4("Probability of Integration"),
      p("The occurrence of multisensory integration depends on the first stage
        processing times \\(V\\) and \\(A\\), the stimulus onset asynchrony
        (SOA, denoted by \\(\\tau\\)), and the width of the
        Time-Window of Integration, denoted by \\(\\omega\\)."),
      p("The SOA is defined such that the visual stimulus is always presented
        at \\(\\tau = 0\\). Thus, \\(\\tau > 0\\) indicates that that the
        visual is presented before the auditory stimulus, and \\(\\tau < 0\\)
        indicates the reverse presentation order."),
      p("For the different paradigms, the event of multisensory integration
        is defined somewhat differently."),
      withTags({
        # Panel Collapse Accordion
        div(class="panel-group", id="accordionPI",
        # FAP
          div(class="panel panel-default",
            div(class="panel-heading",
              h5(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordionPI",
                  href="#collapseFAP", "Focused Attention Paradigm"))),
            div(id="collapseFAP", class="panel-collapse collapse in",
              div(class="panel-body",
                img(src="fap.jpeg", width = "25%"), br(),
                p("For FAP, the event of multisensory integration
                   is defined as
                  $$I_{FAP} = \\{A + \\tau < V < A + \\tau + \\omega\\}.$$
                  The probability of integration can therefore be calculated as
                  $$\\begin{align}
                      P(I_{\\tau, \\omega}) &= P(A + \\tau < V < A + \\tau +
                                                  \\omega) \\\\
                                            &= \\int_{0}^{\\infty} \\!
                                            f_A(x)\\{F_V(x + \\tau + \\omega) -
                                                F_V(x+\\tau)\\}\\,\\mathrm{d}x
                  \\end{align}$$
                  where the solutions of the integral depend on the sign of
                  \\(\\omega\\) + \\(\\tau\\).",
                  a(href="#pifap", 'data-toggle'="collapse",
                    "See solutions of the integral."),
                  div(id="pifap", class="collapse",
                    p("(i) \\(\\tau\\) < \\(\\tau\\) + \\(\\omega\\) < 0
                      $$\\begin{align}
                        P(I_{\\tau,\\omega}) &= \\int_{-\\tau-\\omega}^{-\\tau} \\!
                      \\lambda_{A}e^{-\\lambda_{A}x}{1-e^{-\\lambda_V(x+\\tau+\\omega)}}
                      \\, \\mathrm{d}x -\\int_{-\\tau}^{\\infty} \\!
                      \\lambda_{A}e^{-\\lambda_{A}x}{e^{-\\lambda_V(x+\\tau)} -
                        e^{-\\lambda_V(x+\\tau+\\omega)}} \\, \\mathrm{d}x \\\\
                      &= \\frac{\\lambda_V}{\\lambda_V+\\lambda_A}
                      e^{\\lambda_A\\tau} (-1 + e^{\\lambda_A\\omega});
                      \\end{align}$$",
                      "(ii)  \\(\\tau\\) < 0 < \\(\\tau\\) + \\(\\omega\\)
                      $$\\begin{align}
                        P(I_{\\tau,\\omega})  &= \\int_{0}^{-\\tau} \\!
                        \\lambda_{A}e^{-\\lambda_{A}x}\\{1-e^{-\\lambda_V(x+\\tau+\\omega)}\\}
                        \\, \\mathrm{d}x + \\int_{-\\tau}^{\\infty} \\!
                        \\lambda_{A}e^{-\\lambda_{A}x}\\{e^{-\\lambda_V(x+\\tau)} -
                            e^{-\\lambda_V(x+\\tau+\\omega)}\\} \\, \\mathrm{d}x \\\\
                        &= \\frac{1}{\\lambda_V+\\lambda_A}
                        \\{\\lambda_A(1-e^{-\\lambda_V(\\omega+\\tau)})+
                            \\lambda_V(1-e^{\\lambda_A\\tau})\\};
                      \\end{align}$$",
                      "(iii) \\(0 < \\tau < \\tau + \\omega\\)
                      $$\\begin{align}
                        P(I_{\\tau,\\omega}) &= \\int_{0}^{\\infty} \\!
                        \\lambda_{A}e^{-\\lambda_{A}x}\\{e^{-\\lambda_V(x+\\tau)} -
                          e^{-\\lambda_V(x+\\tau+\\omega)}\\}
                        \\, \\mathrm{d}x \\\\
                        &= \\frac{\\lambda_A}{\\lambda_V+\\lambda_A}
                        \\{e^{-\\lambda_V(\\tau)}-e^{-\\lambda_V(\\omega+\\tau)}.
                      \\end{align}$$")
            )))
          )),
        # RTP
          div(class="panel panel-default",
            div(class="panel-heading",
              h5(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordionPI",
                  href="#collapseRTP", "Redundant Target Paradigm"))),
            div(id="collapseRTP", class="panel-collapse collapse",
              div(class="panel-body",
                img(src="rtp.jpeg", width = "25%"), br(),
                p("For RTP, multisensory integration occurs only in two events,
                  which are united in $$I_{RTP} = \\{A + \\tau < V < A + \\tau
                  + \\omega\\} \\cup \\{V < A + \\tau < V + \\omega\\},$$",
                  "The probability of integration \\(P(I_{RTP})\\) is computed
                  as the sum of the probabilities of the two events above,
                  \\(P(I_{RTP}) = p_1 + p_2\\). In the first case, the",
                  strong("acoustic stimulus wins"), "the race in the first
                  stage: $$p_1 = P(A + \\tau < V < A + \\tau + \\omega) =
                      \\int_0^{\\infty}\\{F_V(a + \\tau + \\omega) - F_V(a +
                                                                         \\tau)\\}
                  \\,\\mathrm{d}F_A(a).$$ In the second case the",
                  strong("visual stimulus wins"), ": $$p_2 = P(V < A + \\tau <
                  V + \\omega) = \\int_0^{\\infty}\\{F_A(v + \\omega - \\tau) -
                  F_A(v - \\tau)\\} \\,\\mathrm{d}F_V(v).$$ The values of these
                  integrals depend on signs of \\(\\tau\\) and \\(\\omega\\) +
                  \\(\\tau\\).",
                  a(href="#pirtp", 'data-toggle'="collapse",
                    "See solutions of the integrals."),
                  div(id="pirtp", class="collapse",
                    tags$ul(
                      tags$li("For \\(\\tau < 0\\) (which means the",
                              strong("acoustic stimulus is presented first),"),
                        "$$p_1 = \\begin{cases}
                            \\frac{\\lambda_V}{\\lambda_V + \\lambda_A}
                        \\left\\{\\exp(\\lambda_A(\\tau + \\omega)) -
                            \\exp(\\lambda_A \\tau)\\right\\} & \\quad
                        \\text{if } \\tau + \\omega < 0,\\\\
                            \\frac{\\lambda_A}{\\lambda_V + \\lambda_A}
                        \\left\\{1 - \\exp(-\\lambda_V(\\tau +
                                                       \\omega))\\right\\} +
                              \\frac{\\lambda_V}{\\lambda_V + \\lambda_A}
                          \\left\\{1 - \\exp(\\lambda_A \\tau)\\right\\} &
                              \\quad \\text{if } \\tau < 0 < \\tau + \\omega,
                        \\end{cases}$$
                        and
                        $$p_2 = \\frac{\\lambda_V}{\\lambda_V + \\lambda_A}
                        \\left\\{ \\exp(\\lambda_A \\tau) -
                            \\exp(-\\lambda_A(\\omega - \\tau))\\right\\}.$$"),
                      tags$li("For \\(\\tau > 0\\) (which means the",
                              strong("visual stimulus is presented first),"),
                        "$$p_1 = \\frac{\\lambda_A}{\\lambda_V + \\lambda_A}
                        \\left\\{ \\exp(-\\lambda_V \\tau) -
                            \\exp(-\\lambda_V(\\omega + \\tau))\\right\\},$$
                        and
                        $$p_2 = \\begin{cases}
                            \\frac{\\lambda_A}{\\lambda_V + \\lambda_A}
                        \\left\\{\\exp(\\lambda_V(\\omega - \\tau)) -
                            \\exp(-\\lambda_V \\tau)\\right\\} & \\quad
                        \\text{if } \\tau > \\omega,\\\\
                            \\frac{\\lambda_A}{\\lambda_V + \\lambda_A}
                        \\left\\{1 - \\exp(-\\lambda_V \\tau)\\right\\} +
                            \\frac{\\lambda_V}{\\lambda_V + \\lambda_A}
                        \\left\\{1 - \\exp(-\\lambda_A(\\omega -
                                                       \\tau))\\right\\} &
                              \\quad \\text{if } \\tau < \\omega.
                        \\end{cases}$$")
                  ))
            )))
    ))})),
    "Parameter Estimation",
    tabPanel("Minimum Least Squares",
      h4("Parameter Estimation with Minimum Least Squares"),
      h5("The Objective Function"),
      p("Parameters are estimated by minimizing the
        \\(\\chi^2\\) statistic:
        $$\\begin{align}
          \\chi^2 = \\sum_{\\text{all
          conditions}}[\\frac{\\text{mean}[RT_{obs}]-[RT_{pred}]}{\\text{standard
                     error}[RT_{obs}]}]^2
        \\end{align},$$
        with \\(RT_{obs}\\) and \\(RT_{pred}\\) are the observed and predicted
        reaction times, respectively."),
      h5("Parameter bounds"),
      p("The following boundaries are set for the parameters:"),
      fluidRow(
        column(4, offset=4,
          withTags({
            table(width="50%", class="table table-striped table-condensed",
              tr(th("Parameter"), th("Upper Bound"), th("Lower Bound")),
              tr(td("\\(^1/_{\\lambda_A}\\)"), td("5"), td("250")),
              tr(td("\\(^1/_{\\lambda_V}\\)"), td("5"), td("250")),
              tr(td("\\(\\mu\\)"), td("0"), td("Inf")),
              tr(td("\\(\\omega\\)"), td("5"), td("1000")),
              tr(td("\\(\\Delta\\)"), td("0"), td("175")))
      })))
    ),
    widths = c(2, 10)
)),
  column(1,
    h5("Glossary", style = "color:black"),

    bsButton("glossOmega", "\\(\\omega\\)"),
    bsPopover(id="glossOmega", title="&#969 (omega)",
              content="width of the time-window of integration",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glossrtva", "\\(RT_{VA}\\)"),
    bsPopover(id="glossrtva", title="RT<sub>VA</sub>",
              content="reaction time in the crossmodal condition",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glossm1", "\\(M_{1}\\)"),
    bsPopover(id="glossm1", title="M<sub>1</sub>",
              content="first stage processing time",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glossm2", "\\(M_{2}\\)"),
    bsPopover(id="glossm2", title="M<sub>2</sub>",
              content="second stage processing time",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glossi", "\\(I\\)"),
    bsPopover(id="glossi", title="I",
              content="event that multisensory integration occurs",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glosspi", "\\(P(I)\\)"),
    bsPopover(id="glosspi", title="P(I)",
              content="probability of multisensory integration",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glossDelta", "\\(\\Delta\\)"),
    bsPopover(id="glossDelta", title="&#916 (delta)",
              content="size of the crossmodal interaction effect",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glossav", "\\(V\\), \\(A\\)"),
    bsPopover(id="glossav", title="V, A",
              content="first stage processing time of the visual / auditory stimulus",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glosslambdaav", "\\(\\lambda_V\\), \\(\\lambda_A\\)"),
    bsPopover(id="glosslambdaav", title="&#955<sub>V</sub>, &#955<sub>A</sub>",
              content="rate parameter of the exponential distribution of the first stage processing time for the visual / auditory stimulus",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glossMu", "\\(\\mu\\)"),
    bsPopover(id="glossMu", title="&#956 (mu)",
              content="expected second stage reaction time in the unimodal condition",
              placement="bottom", trigger="hover"),
    br(),
    bsButton("glossSOAtau", "SOA / \\(\\tau\\)"),
    bsPopover(id="glossSOAtau", title="SOA / &#964 (tau)",
              content="stimulus onset asynchrony; time between the presentation of the two stimuli",
              placement="bottom", trigger="hover"),
    style='margin-left:0px;border-left:1px solid; padding: 10px; color:#D3D3D3')
))
