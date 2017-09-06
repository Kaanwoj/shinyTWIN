tabPanel("Theory", value = "Theory",
  navlistPanel(
    "Setting",
    tabPanel("Paradigms",
      h3("Experimental Setup"),
        withTags({
          div(class="panel-group",
            div(class="panel panel-default",
              div(class="panel-heading", "Bi-modal condition:"),
              div(class="panel-body",
                "Two (or more) stimuli are presented on different modalities
                simultaneously or with a short delay between the stimuli.")),
            div(class="panel panel-default",
              div(class="panel-heading", "Uni-modal condition:"),
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
    tabPanel("Reaction Times"),
    tabPanel("Parameter Distributions"),
    tabPanel("Probability of Integration")


))
