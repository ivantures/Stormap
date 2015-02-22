# Stormap | ui.R
##################

library(shiny)

shinyUI(navbarPage('Stormap',
	tabPanel('Start Here!',
		fluidRow(
			h1('Stormap: Where a map is worth a thousand rows...'),
			h2('Why Stormap?'),
			p('Storms and other severe weather events can cause both public health (fatalities and injuries) and economic problems (property and crop damages) for communities and municipalities.  Preventing such outcomes to the extent possible is a key concern.'),
			p('The ', a("U.S. National Oceanic and Atmospheric Administration's (NOAA) Storm Database", href = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'),' tracks ', strong('when'), ' and ', strong('where'), ' major storms and weather events occurred in the United States including estimates of ', strong('fatalities'), ', ', strong('injuries'), ', and ', strong('property'), ' and ', strong('crop damages'), '.'),
			p('Clocking at 46+MB, this database is just that: a big database with rows and columns filled with numbers but Stormap aims to make it easier for you to explore this database so you can in turn transform this information into *', strong('insights'), '*.'),
			h2('How do I use Stormap?'),
			p("It's really simple:"),
			h4('Step 0: Click the ', strong('Explore!'), ' button on top of the window.'),
			div(em("(that's the button that just looks like this: ", img(src = 'explore.png'), ')'), align = 'center'),
			h4('Step 1: Narrow down the horizon time, the type of storms, and the impacts of these storms you are interested in:'),
			p(div(img(src = 'input.png'), align = 'center')),
			h4(HTML('<strike>Step 2:</strike>'), "That's it"),
			p('Just give it a few seconds for the map to refresh and you should get something similar to this:'),
			p(div(img(src = 'map.png'), align = 'center')),
			p(div(em('(note: the number in parentheses refer to the number of events)'), align = 'center')),
			p("Finally, tables at the bottom highlight the map's key events:"),
			p(div(img(src = 'tables.png'), align = 'center')),
			h3('Enjoy!'))),
	tabPanel(strong('Explore!'),
		fluidRow(
			column(4, wellPanel(
				selectInput(inputId = 'period',
					label = 'Select a period:',
					choices = c('Loading...')))),
			column(4, wellPanel(
				selectInput(inputId = 'event.type',
					label = 'Select an event type:',
					choices = c('Loading...')))),
			column(4, wellPanel(
				checkboxGroupInput(inputId = 'impact.types',
					label = 'Select impact types:',
					choices = c('Loading...' = 'loading'))))),
		fluidRow(column(10, offset = 1,
			plotOutput('map'))),
		fluidRow(column(3,
				h4('Top fatalities'),
				tableOutput('top.fatalities')),
			column(3,
				h4('Top injuries'),
				tableOutput('top.injuries')),
			column(3,
				h4('Top property damages'),
				tableOutput('top.property.damages')),
			column(3,
				h4('Top crop damages'),
				tableOutput('top.crop.damages'))))))
