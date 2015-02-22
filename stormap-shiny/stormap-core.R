# Stormap | stormap-core.R
############################
# (used by shiny but also by slidify when "slidifying" the presentation)

ALL <- '== ALL =='

# Formatting helper functions
###############################

event.format.count <- function(count) {
	count <- as.numeric(count)
	format(x = count,
		trim = TRUE,
		digits = 1,
		justify = 'none',
		big.mark = ',',
		scientific = FALSE)
}

event.format.dollar <- function(amount) {
	amount <- as.numeric(amount)
	format(x = amount,
		digits = 1,
		justify = 'none',
		big.mark = ',',
		scientific = FALSE)
}

# Load the dataset
####################

ddf_df <- read.table(file = 'stormap-data.txt',
	header = TRUE,
	stringsAsFactors = FALSE)

library(dplyr)

###########
# Periods
###########

periods <- function() {
	year.min = min(ddf_df$year)
	year.max = max(ddf_df$year)

	periods <- matrix(ncol = 3,
		byrow = TRUE,
		data = sapply(X = sort(x = unique(x = round(x = ddf_df$year,
					digit = -1))),
				FUN = function(year) {
					from.year = max(c(year.min, year))
					to.year = min(c(year + 9, year.max))
					label = paste(from.year, 'to', to.year)
					c(from.year,
						to.year,
						label)}))
	rbind(data.frame(from.year = year.min,
			to.year = year.max,
			label = ALL,
			stringsAsFactors = FALSE),
		data.frame(
			from.year = as.numeric(periods[, 1]),
			to.year = as.numeric(periods[, 2]),
			label = periods[, 3],
			stringsAsFactors = FALSE))
}

periods <- periods()

select.period <- function(period.label) {
	periods %>%
		filter(label == period.label)
}

###############
# Event types
###############

event.types <- ddf_df %>%
	group_by(event.type) %>%
	summarise(count = n()) %>%
	arrange(desc(count), event.type) %>%
	dplyr::select(event.type) %>% # !@# MASS:select masking dplyr::select
	mutate(label = event.type)

event.type.labels <- rbind(
	data.frame(event.type = NA,
		label = ALL,
		stringsAsFactors = FALSE),
	event.types)

select.event.types <- function(event.type.label) {
	if (event.type.label == ALL) {
		return (event.types)
	}

	event.types %>%
		filter(label == event.type.label)
}

count.event.types <- function(df) {
	df <- df %>%
		group_by(event.type) %>%
		summarise(count = n()) %>%
		arrange(desc(count), event.type)

	df$label <- apply(X = df,
		MARGIN = 1,
		FUN = function(row) {
			paste0(row[['event.type']], ' (',
				event.format.count(row[['count']]), ')') })

	df
}

################
# Impact types
################

impact.types <- c(
	'Fatalities' = 'Fatalities',
	'Injuries' = 'Injuries',
	'Property damages' = 'Property damages',
	'Crop damages' = 'Crop damages')

count.impact.types <- function(df) {
	df <- df %>%
		group_by(impact.type) %>%
		summarise(count = n()) %>%
		arrange(desc(count), impact.type) %>%
		mutate(count = as.character(count))

	df$label <- apply(X = df,
		MARGIN = 1,
		FUN = function(row) {
			paste0(row[['impact.type']], ' (',
				event.format.count(row[['count']]), ')') })

	df
}

##################################
# Subsetting the selected events
##################################

events.subset <- function(period, event.types, impact.types) {
	df <- ddf_df %>%
		filter(year %in% period$from.year:period$to.year) %>%
		filter(event.type %in% event.types$event.type) %>%
		filter(impact.type %in% impact.types)

	if (nrow(df) == 0) {
		return (df)
	}

	event.types.count <- count.event.types(df)
	impact.types.count <- count.impact.types(df)

	df %>%
		mutate(event.type.factor = factor(x = event.type,
				levels = event.types.count$event.type,
				labels = event.types.count$label,
				ordered = TRUE),
			impact.type.factor = factor(x = impact.type,
				levels = impact.types.count$impact.type,
				labels = impact.types.count$label,
				ordered = TRUE))
}

################################
# Plotting the selected events
################################

library(ggplot2)
library(maps)

events.plot <- function(subset) {
	plot <- ggplot() +
		geom_polygon(data = map_data('state'),
			aes(x = long, y = lat, group = group),
			color = 'grey', fill = 'white') +
		theme(panel.grid.minor = element_blank(),
			panel.grid.major = element_blank(),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank(),
			axis.title.x = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			axis.title.y = element_blank()) +
		ggtitle(label = paste('Storm geolocation between',
			subset$period$from.year,
			' and ',
			subset$period$to.year,
			paste0('(',
				event.format.count(nrow(subset$events)),
				')')))

	if (nrow(subset$events) == 0) {
		return (plot)
	}

	plot + geom_point(data = subset$events,
			aes(x = longitude,
				y = latitude,
				color = event.type.factor,
				shape = impact.type.factor,
				size = impact.value)) +
		scale_color_discrete(name = 'Impact Type') +
		scale_shape_discrete(name = 'Event Type') +
		scale_size(guide = 'none') +
		guides(color = guide_legend(override.aes = list(shape = 12, size = 4), order = 1),
			shape = guide_legend(override.aes = list(size = 4), order = 2))
}

##########
# Tables
##########

top.fatalities.table <- function(subset) {
	df <- subset$events %>%
		filter(impact.type == 'Fatalities') %>%
		arrange(desc(impact.value), year, state) %>%
		head(n = 10) %>%
		dplyr::select(c(year, event.type, impact.value, state)) %>%
		mutate(impact.value = event.format.count(impact.value)) %>%
		rename('Year' = year,
			'Event type' = event.type,
			"Fatalities\n(persons)" = impact.value,
			'State' = state)

	if (nrow(df) == 0) {
		return (data.frame())
	}

	df
}

top.injuries.table <- function(subset) {
	df <- subset$events %>%
		filter(impact.type == 'Injuries') %>%
		arrange(desc(impact.value), year, state) %>%
		head(n = 10) %>%
		dplyr::select(c(year, event.type, impact.value, state)) %>%
		mutate(impact.value = event.format.count(impact.value)) %>%
		rename('Year' = year,
			'Event type' = event.type,
			"Injuries\n(persons)" = impact.value,
			'State' = state)

	if (nrow(df) == 0) {
		return (data.frame())
	}

	df
}

top.property.damages.table <- function(subset) {
	df <- subset$events %>%
		filter(impact.type == 'Property damages') %>%
		arrange(desc(impact.value), year, state) %>%
		head(n = 10) %>%
		dplyr::select(c(year, event.type, impact.value, state)) %>%
		mutate(impact.value =
			event.format.dollar(impact.value / (10 ^ 9))) %>%
		rename('Year' = year,
			'Event type' = event.type,
			"Property\ndamages\n($B)" = impact.value,
			'State' = state)

	if (nrow(df) == 0) {
		return (data.frame())
	}

	df
}

top.crop.damages.table <- function(subset) {
	df <- subset$events %>%
		filter(impact.type == 'Crop damages') %>%
		arrange(desc(impact.value), year, state) %>%
		head(n = 10) %>%
		dplyr::select(c(year, event.type, impact.value, state)) %>%
		mutate(impact.value =
			event.format.dollar(impact.value / (10 ^ 9))) %>%
		rename('Year' = year,
			'Event type' = event.type,
			"Crop\ndamages\n($B)" = impact.value,
			'State' = state)

	if (nrow(df) == 0) {
		return (data.frame())
	}

	df
}
