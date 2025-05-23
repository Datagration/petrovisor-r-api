% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataServices.R
\name{DataServices}
\alias{DataServices}
\title{DataServices}
\description{
Provides access to data related functionality provided through
the web API.
}
\details{
A new instance of this class will be created by the ServiceProvider
automatically.
}
\examples{
\dontrun{
# create a new instance of the service provider
sp <- ServiceProvider$new(
  url = discovery_url,
  workspace = workspace,
  user = user,
  password = password
)

# load data
data <- sp$data$load(
  c("entity_one", "entity_two"),
  lapply(
    c("test time string signal one [ ]",
      "test time string signal two [ ]"),
    function(x) {
      sp$parse_signal(x)
    }
  ),
  time_increment = "Daily",
  time_start = "2025-01-01T00:00:00",
  time_end = "2025-01-02T00:00:00",
  reshape = TRUE
)

# save data
time_numeric_data <- data.frame(
  c("", ""),
  c("2025-01-01T00:00:00", "2025-01-02T00:00:00"),
  c("entity_one", "entity_two"),
  c(23.45, 45.56),
  c(78.89, NaN)
)

colnames(time_numeric_data) <- c("scenario",
                                 "date",
                                 "entity",
                                 time_numeric_signal_1_name,
                                 time_numeric_signal_2_name)

result <- sp$data$save(
  "TimeNumeric",
  time_numeric_data,
  signals = lapply(
    c("test time numeric signal one [m3]",
      "test time numeric signal two [m3]"),
    function(x) {
      sp$parse_signal(x)
    }
  )
)
}
}
\section{Super class}{
\code{\link[Myrconn.PetroVisor.Client:ApiRequests]{Myrconn.PetroVisor.Client::ApiRequests}} -> \code{DataServices}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DataServices-new}{\code{DataServices$new()}}
\item \href{#method-DataServices-load}{\code{DataServices$load()}}
\item \href{#method-DataServices-save}{\code{DataServices$save()}}
\item \href{#method-DataServices-delete}{\code{DataServices$delete()}}
\item \href{#method-DataServices-clone}{\code{DataServices$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataServices-new"></a>}}
\if{latex}{\out{\hypertarget{method-DataServices-new}{}}}
\subsection{Method \code{new()}}{
Create a new DataServices instance. This is done by the
ServiceProvider automatically.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataServices$new(url, token_type, token)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{url}}{the URL for the API calls.}

\item{\code{token_type}}{the type of the issued token.}

\item{\code{token}}{the issued token.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataServices-load"></a>}}
\if{latex}{\out{\hypertarget{method-DataServices-load}{}}}
\subsection{Method \code{load()}}{
Load data from PetroVisor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataServices$load(
  entities,
  signals,
  scenario_names = NULL,
  hierarchy_name = NULL,
  top_records = NULL,
  include_workspace_data = TRUE,
  time_increment = NULL,
  time_start = NULL,
  time_end = NULL,
  depth_increment = NULL,
  depth_start = NULL,
  depth_end = NULL,
  with_gaps = TRUE,
  gap_numeric_value = NULL,
  gap_string_value = NULL,
  depth_unit = NULL,
  pressure_unit = NULL,
  temperature_unit = NULL,
  aggregation = NULL,
  reshape = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{entities}}{List of entities to retrieve data for. Either a list of
strings or a list of items of type Entity.}

\item{\code{signals}}{List of parsed signals to retrieve data for.}

\item{\code{scenario_names}}{List of scenario names to load data for.}

\item{\code{hierarchy_name}}{Hierarchy used in the data retrieval process.}

\item{\code{top_records}}{Number of records to return,}

\item{\code{include_workspace_data}}{Whether workspace data shall be included in
the output (only applies if scenarios are used). Defaults to
\code{TRUE}.}

\item{\code{time_increment}}{The time increment to load the data in.}

\item{\code{time_start}}{The first time stamp data is loaded for.}

\item{\code{time_end}}{The last time stamp data is loaded for.}

\item{\code{depth_increment}}{The depth increment to load the data in.}

\item{\code{depth_start}}{The first depth data is loaded for.}

\item{\code{depth_end}}{The last depth data is loaded for.}

\item{\code{with_gaps}}{Whether gaps shall be returned. Defaults to \code{TRUE}.}

\item{\code{gap_numeric_value}}{Replacement value for gaps in numeric data.}

\item{\code{gap_string_value}}{Replacement value for gaps in string data.}

\item{\code{depth_unit}}{The depth unit for retrieving depth data.}

\item{\code{pressure_unit}}{The pressure unit used when retrieving PVT data.}

\item{\code{temperature_unit}}{The temperature unit used when retrieving PVT
data.}

\item{\code{aggregation}}{Aggregation type applied to the data.}

\item{\code{reshape}}{Whether to return the raw output of the api call or
reshape the data into a more user-friendly format. Defaults to
\code{TRUE}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataServices-save"></a>}}
\if{latex}{\out{\hypertarget{method-DataServices-save}{}}}
\subsection{Method \code{save()}}{
Save data to PetroVisor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataServices$save(
  data_type = c("StaticNumeric", "StaticString", "TimeNumeric", "TimeString",
    "DepthNumeric", "DepthString", "PVTNumeric"),
  data,
  signals,
  generate_logs = TRUE,
  no_range_delete = TRUE,
  values_time_increment = NULL,
  values_depth_increment = NULL,
  pressure_unit = NULL,
  temperature_unit = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_type}}{The type of the data. One of:
\code{StaticNumeric}, \code{StaticString}, \code{TimeNumeric},
\code{TimeString}, \code{DepthNumeric}, \code{DepthString},
\code{PVTNumeric}.}

\item{\code{data}}{The data as data frame.}

\item{\code{signals}}{List of parsed signals to save data for.}

\item{\code{generate_logs}}{Whether to generate log entries. Defaults to
\code{TRUE}.}

\item{\code{no_range_delete}}{Whether to skip deleting all data in the saving
scope before storing the new values. Defaults to \code{TRUE}.}

\item{\code{values_time_increment}}{The time increment of the time-dependent
values.}

\item{\code{values_depth_increment}}{The depth increment of the depth-dependent
values.}

\item{\code{pressure_unit}}{The pressure unit of the PVT values.}

\item{\code{temperature_unit}}{The temperature unit of the PVT values.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataServices-delete"></a>}}
\if{latex}{\out{\hypertarget{method-DataServices-delete}{}}}
\subsection{Method \code{delete()}}{
Remove data from PetroVisor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataServices$delete(
  entities,
  signal_names,
  scenario_names = NULL,
  include_workspace_data = TRUE,
  time_start = NULL,
  time_end = NULL,
  depth_start = NULL,
  depth_end = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{entities}}{List of entities to delete data for. Either a list of
strings or a list of items of type Entity.}

\item{\code{signal_names}}{List of signal names to delete data for.}

\item{\code{scenario_names}}{List of scenario names to delete data from.}

\item{\code{include_workspace_data}}{Whether workspace data shall be deleted as
well (only applies if scenarios are used). Defaults to \code{TRUE}.}

\item{\code{time_start}}{The first time stamp data is deleted for.}

\item{\code{time_end}}{The last time stamp data is deleted for.}

\item{\code{depth_start}}{The first depth data is deleted for.}

\item{\code{depth_end}}{The last depth data is deleted for.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataServices-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DataServices-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataServices$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
