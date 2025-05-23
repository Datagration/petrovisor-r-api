% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataIntegration.R
\name{DataIntegrationSet}
\alias{DataIntegrationSet}
\title{DataIntegrationSet}
\description{
Class representing a data integration set.
}
\examples{
\dontrun{
DataIntegrationSet$new()
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{The name of the data integration set.}

\item{\code{overwriteExistingData}}{This flag specifies whether existing data
shall be overwritten by data extracted from the source.}

\item{\code{trackNewEntities}}{This flag specifies whether new entity shall be
tracked in the log.}

\item{\code{dataSourceNames}}{A list of all data sources of the data entegration
set (names only).}

\item{\code{cleansingScriptNames}}{A list of all cleansing scripts applied to
the data (names only).}

\item{\code{timeStep}}{The time step for the data integration. One of:
\code{"EveryMinute"}, \code{"EveryFiveMinutes"},
\code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
\code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}.}

\item{\code{depthStep}}{The depth step for the data integration. One of:
\code{"Every tenth of meter"}, \code{"Every eighth of meter"},
\code{"Every half of foot"}, \code{"Every foot"},
\code{"Every half of meter"}, \code{"Every meter"}.}

\item{\code{timeStart}}{The start date for the data aquisition.}

\item{\code{timeEnd}}{The end date for the data aquisition.}

\item{\code{depthStart}}{The start depth for the data aquisition.}

\item{\code{depthEnd}}{The end depth for the data aquisition.}

\item{\code{isLocked}}{This flag specifies whether the data integration set is
locked. Defaults to \code{FALSE}.}

\item{\code{user}}{The user the data integration set belongs to.}

\item{\code{isFavorite}}{This flag specifies whether the data integration set is
marked as favorite item, and thus shown in the favorites tab on the home
module in PetroVisor. Defaults to \code{FALSE}.}

\item{\code{labels}}{A list of strings holding the labels of the data integration
set.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DataIntegrationSet-new}{\code{DataIntegrationSet$new()}}
\item \href{#method-DataIntegrationSet-toList}{\code{DataIntegrationSet$toList()}}
\item \href{#method-DataIntegrationSet-clone}{\code{DataIntegrationSet$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataIntegrationSet-new"></a>}}
\if{latex}{\out{\hypertarget{method-DataIntegrationSet-new}{}}}
\subsection{Method \code{new()}}{
Create a new DataIntegrationSet instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataIntegrationSet$new(
  name = NULL,
  overwriteExistingData = NULL,
  trackNewEntities = NULL,
  dataSourceNames = NULL,
  cleansingScriptNames = NULL,
  timeStep = c("EveryMinute", "EveryFiveMinutes", "EveryFifteenMinutes", "Hourly",
    "Daily", "Monthly", "Quarterly", "Yearly"),
  depthStep = c("Every tenth of meter", "Every eighth of meter", "Every half of foot",
    "Every foot", "Every half of meter", "Every meter"),
  timeStart = NULL,
  timeEnd = NULL,
  depthStart = NULL,
  depthEnd = NULL,
  isLocked = FALSE,
  user = NULL,
  isFavorite = FALSE,
  labels = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the data integration set.}

\item{\code{overwriteExistingData}}{This flag specifies whether existing data
shall be overwritten by data extracted from the source.}

\item{\code{trackNewEntities}}{This flag specifies whether new entity shall be
tracked in the log.}

\item{\code{dataSourceNames}}{A list of all data sources of the data entegration
set (names only).}

\item{\code{cleansingScriptNames}}{A list of all cleansing scripts applied to
the data (names only).}

\item{\code{timeStep}}{The time step for the data integration. One of:
\code{"EveryMinute"}, \code{"EveryFiveMinutes"},
\code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
\code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}.}

\item{\code{depthStep}}{The depth step for the data integration. One of:
\code{"Every tenth of meter"}, \code{"Every eighth of meter"},
\code{"Every half of foot"}, \code{"Every foot"},
\code{"Every half of meter"}, \code{"Every meter"}.}

\item{\code{timeStart}}{The start date for the data aquisition.}

\item{\code{timeEnd}}{The end date for the data aquisition.}

\item{\code{depthStart}}{The start depth for the data aquisition.}

\item{\code{depthEnd}}{The end depth for the data aquisition.}

\item{\code{isLocked}}{This flag specifies whether the data integration set is
locked. Defaults to \code{FALSE}.}

\item{\code{user}}{The user the data integration set belongs to.}

\item{\code{isFavorite}}{This flag specifies whether the data integration set is
marked as favorite item, and thus shown in the favorites tab on the home
module in PetroVisor. Defaults to \code{FALSE}.}

\item{\code{labels}}{A list of strings holding the labels of the data integration
set.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataIntegrationSet-toList"></a>}}
\if{latex}{\out{\hypertarget{method-DataIntegrationSet-toList}{}}}
\subsection{Method \code{toList()}}{
Convert the object to a list. This function is mainly used
by the Services to convert the objects to lists and then
call the web API.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataIntegrationSet$toList()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataIntegrationSet-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DataIntegrationSet-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataIntegrationSet$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
