tabPanel("Theory", value = "Theory",
  navlistPanel(
    "Setting",
    tabPanel("Paradigms",
      h3("Experimental Setup"),
        withTags({
          div(class="panel-group",
            div(class="panel panel-default",
              div(class="panel-heading", "Crossmodal condition:"),
              div(class="panel-body",
                "Two (or more) stimuli are presented on different modalities
                simultaneously or with a short delay between the stimuli.")),
            div(class="panel panel-default",
              div(class="panel-heading", "Unimodal condition:"),
              div(class="panel-body",
                "One stimulus is presented on one modality."))
        )}),
      h3("Participant's Task"),
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
      p("Multisensory integration of sensory information into a
        perceptual unit ('event') occurs only if the peripheral sensory
        processes all terminate within a given temporal interval, the
        'time window of integration'."),
      img(src="TWIN.png", width = "60%"), br(),
      withTags({
        # Panel Collapse Accordion
        div(class="panel-group", id="accordion",
        # First Stage Assumption
          div(class="panel panel-default",
            div(class="panel-heading",
              h4(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordion",
                  href="#collapse1", "First Stage Assumption"))),
            div(id="collapse1", class="panel-collapse collapse in",
              div(class="panel-body",
                "The first stage consists in a (stochastically independent)
                race among the peripheral processes in the visual and auditory
                pathways triggered by a crossmodal stimulus complex."))),
        # Second Stage Assumption
          div(class="panel panel-default",
            div(class="panel-heading",
              h4(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordion",
                  href="#collapse2", "Second Stage Assumption"))),
            div(id="collapse2", class="panel-collapse collapse",
              div(class="panel-body",
                "The second stage comprises all processes following the first
                stage including preparation and execution of a response."))),
        # Time-Window of Integration Assumption"
          div(class="panel panel-default",
            div(class="panel-heading",
              h4(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordion",
                  href="#collapse3", "Time-Window of Integration Assumption"))),
            div(id="collapse3", class="panel-collapse collapse",
              div(class="panel-body",
                "Multisensory integration occurs only if the peripheral
                processes of the first stage all terminate within a given
                temporal interval, the time window of integration."))),
        # Temporal Separability Assumption
          div(class="panel panel-default",
            div(class="panel-heading",
              h4(class="panel-title",
                a('data-toggle'="collapse", 'data-parent'="#accordion",
                  href="#collapse4", "Assumption of Temporal Separability"))),
            div(id="collapse4", class="panel-collapse collapse",
              div(class="panel-body",
                "The amount of interaction, manifesting itself in an increase
                or decrease of second stage processing time, is a function of
                crossmodal stimulus features, but it does not depend on the
                presentation asynchrony (SOA) of the stimuli.")))
    )})),
    tabPanel("Reaction Times",
      p("The total reaction time in the crossmodal condition (\\(RT_{VA})\\) is
        the sum of the processing times on stage 1 and stage 2. Let \\(M1\\)
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
      h3("First Stage"),
      p("Random variable \\(M_1\\) refers to the peripheral processing time
        on stage 1. For the processing times for the visual and the
        acoustic stimulus, ", strong("V and A denote the two statistically
        independent, exponentially distributed random variables.")),
      a(href="#densityfirst", 'data-toggle'="collapse",
        "See density function"),
      div(id="densityfirst", class="collapse",
        p("The density function is then
        $$f_{\\lambda}(t) =
            \\begin{cases}
            \\lambda e^{-\\lambda t} & \\quad \\text{if } t>0 \\\\
            0 & \\quad \\text{if } t\\leq 0,
            \\end{cases}$$
        with \\(\\lambda > 0\\) and specific values \\(\\lambda \\equiv
        \\lambda_V\\) and \\(\\lambda \\equiv \\lambda_A\\) for visual and
        auditory modality.")),
      h3("Second Stage"),
      p("Random variable \\(M_2\\) refers to the processing time on stage to
        and is assumed to be ", strong("normally distributed"), "with mean
        \\(\\mu - \\Delta\\) in the crossmodal condition and mean \\(\\mu\\) in
        the unimodal condition."),
      h3("Expected Reaction Times"),
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
    tabPanel("Probability of Integration")


))
