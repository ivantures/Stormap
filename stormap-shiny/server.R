# Stormap | server.R
######################

library(shiny)

source('stormap-core.R')

shinyServer(function(input, output, session) {
	# Input section
	observe({
		updateSelectInput(session = session,
			inputId = 'period',
			choices = periods$label)
	})

	observe({
		updateSelectInput(session = session,
			inputId = 'event.type',
			choices = event.type.labels$label)
	})

	observe({
		updateCheckboxGroupInput(session = session,
			inputId = 'impact.types',
			choices = impact.types,
			selected = impact.types)
	})

	# Reactively update the subset of events the user is selecting
	datasetInput <- reactive({
		period <- select.period(ifelse(input$period == 'Loading...',
			ALL,
			input$period))

		event.types <- select.event.types(
			ifelse(input$event.type == 'Loading...',
				ALL,
				input$event.type))

		impact.types <- input$impact.types

		events <- events.subset(period, event.types, impact.types)

		list(period = period,
			event.types = event.types,
			impact.types = impact.types,
			events = events)
	})

	# Map section
	output$map <- renderPlot({
		events.plot(datasetInput())
	})

	output$top.fatalities <- renderTable({
		top.fatalities.table(datasetInput()) },
		include.rownames = FALSE,
		align = c('c', 'l', 'l', 'r', 'l'))

	# Table section
	output$top.injuries <- renderTable({
		top.injuries.table(datasetInput()) },
		include.rownames = FALSE,
		align = c('c', 'l', 'l', 'r', 'l'))

	output$top.property.damages <- renderTable({
		top.property.damages.table(datasetInput()) },
		include.rownames = FALSE,
		align = c('c', 'l', 'l', 'r', 'l'))

	output$top.crop.damages <- renderTable({
		top.crop.damages.table(datasetInput()) },
		include.rownames = FALSE,
		align = c('c', 'l', 'l', 'r', 'l'))
})
