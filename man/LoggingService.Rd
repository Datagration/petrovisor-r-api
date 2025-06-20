% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LoggingService.R
\name{LoggingService}
\alias{LoggingService}
\title{LoggingService}
\description{
Provides access to logging functionality provided through the
web API.
}
\details{
A new instance of this class will be created by the ServiceProvider
automatically.
}
\examples{
\dontrun{
# create a new instance of the service provider
sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")

# get available categories
availableCategories <- sp$logs$GetAvailableCategories()

# get log entries
allLogEntries <- sp$logs$GetLogEntries()
warnings <- sp$logs$GetLogEntries(severity = "Warning")
signIns <- sp$logs$GetLogEntries(category = "SignIn")

# add log entry
entry <- LogEntry$new(message = "Test",
                      category = "Tag",
                      severity = "Information")
sp$logs$AddLogEntry(entry)

# add several log entries at once
entry1 <- LogEntry$new(message = "Test1",
                       category = "Tag",
                       severity = "Information")
entry2 <- LogEntry$new(message = "Test2",
                       category = "Tag",
                       severity = "Information")
sp$logs$AddLogEntries(list(entry1, entry2))

# remove all log entries
sp$logs$CleanUpLogEntries()
}
}
\section{Super class}{
\code{\link[Myrconn.PetroVisor.Client:ApiRequests]{Myrconn.PetroVisor.Client::ApiRequests}} -> \code{LoggingService}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LoggingService-new}{\code{LoggingService$new()}}
\item \href{#method-LoggingService-load_categories}{\code{LoggingService$load_categories()}}
\item \href{#method-LoggingService-load}{\code{LoggingService$load()}}
\item \href{#method-LoggingService-save}{\code{LoggingService$save()}}
\item \href{#method-LoggingService-delete}{\code{LoggingService$delete()}}
\item \href{#method-LoggingService-clone}{\code{LoggingService$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LoggingService-new"></a>}}
\if{latex}{\out{\hypertarget{method-LoggingService-new}{}}}
\subsection{Method \code{new()}}{
Create a new LoggingService instance. This is done by
the ServiceProvider automatically.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LoggingService$new(url, token_type, token)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-LoggingService-load_categories"></a>}}
\if{latex}{\out{\hypertarget{method-LoggingService-load_categories}{}}}
\subsection{Method \code{load_categories()}}{
Get all available categories from the existing log
entries.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LoggingService$load_categories()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A character vector containing all available categories.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LoggingService-load"></a>}}
\if{latex}{\out{\hypertarget{method-LoggingService-load}{}}}
\subsection{Method \code{load()}}{
Retrieve all log entries matching the given filter from
the database.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LoggingService$load(
  last_entries = NULL,
  start = NULL,
  end = NULL,
  categories = NULL,
  user_name = NULL,
  severities = NULL,
  message_text_filter = NULL,
  workflow = NULL,
  schedule = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{last_entries}}{Return only the latest n tag entries.}

\item{\code{start}}{Return log entries after the specified date (inclusive).}

\item{\code{end}}{Return log entries before the specified date (inclusive).}

\item{\code{categories}}{Return log entries of the specified categaories.}

\item{\code{user_name}}{Return log entries of the specified user.}

\item{\code{severities}}{Return log entries of the given severities.}

\item{\code{message_text_filter}}{Return log entries whose massage
contains the specified text.}

\item{\code{workflow}}{Return log entries for the specified workflow.}

\item{\code{schedule}}{Return log entries for the specified schedule.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A dataframe containing the requested log entries. The number
of returned columns depends on the available information.
Columns that contain no information are not shown in the output.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LoggingService-save"></a>}}
\if{latex}{\out{\hypertarget{method-LoggingService-save}{}}}
\subsection{Method \code{save()}}{
Add one or several log entries to the database at once.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LoggingService$save(log_entries)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{log_entries}}{a list of LogEntry-objects.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LoggingService-delete"></a>}}
\if{latex}{\out{\hypertarget{method-LoggingService-delete}{}}}
\subsection{Method \code{delete()}}{
Remove log entries from the database.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LoggingService$delete(days_to_keep = 0, category)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{days_to_keep}}{Keep the log entries of the last n days in the
log. Defaults to 0.}

\item{\code{category}}{Remove log entries of the specified category only.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LoggingService-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LoggingService-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LoggingService$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
