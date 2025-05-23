% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GeoDataGrid.R
\name{GeoDataGrid}
\alias{GeoDataGrid}
\title{GeoDataGrid}
\description{
Class representing a PetroVisor GeoDataGrid object.
}
\examples{
\dontrun{
GeoDataGrid$new( )
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{The name of the geo data grid.}

\item{\code{gridData}}{A list of values giving the data in the grid.}

\item{\code{rows}}{The number of rows in the grid.}

\item{\code{columns}}{The number of columns in the grid.}

\item{\code{minX}}{The x-index of the grid's min-value.}

\item{\code{minY}}{The y-index of the grid's min-value.}

\item{\code{maxX}}{The x-index of the grid's max-value.}

\item{\code{maxY}}{The y-index of the grid's max-value.}

\item{\code{description}}{A description of the geo data grid.}

\item{\code{isLocked}}{This flag specifies whether the geo data grid is locked.
Defaults to \code{FALSE}.}

\item{\code{user}}{The user the geo data grid belongs to.}

\item{\code{isFavorite}}{This flag specifies whether the geo data grid is marked as
favorite item, and thus shown in the favorites tab on the home module in
PetroVisor. Defaults to \code{FALSE}.}

\item{\code{labels}}{A list of strings holding the labels of the geo data grid.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GeoDataGrid-new}{\code{GeoDataGrid$new()}}
\item \href{#method-GeoDataGrid-toList}{\code{GeoDataGrid$toList()}}
\item \href{#method-GeoDataGrid-clone}{\code{GeoDataGrid$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoDataGrid-new"></a>}}
\if{latex}{\out{\hypertarget{method-GeoDataGrid-new}{}}}
\subsection{Method \code{new()}}{
Create a new GeoDataGrid instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoDataGrid$new(
  name = NULL,
  gridData = NULL,
  rows = NULL,
  columns = NULL,
  minX = NULL,
  minY = NULL,
  maxX = NULL,
  maxY = NULL,
  description = NULL,
  isLocked = FALSE,
  user = NULL,
  isFavorite = FALSE,
  labels = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the geo data grid.}

\item{\code{gridData}}{A list of values giving the data in the grid.}

\item{\code{rows}}{The number of rows in the grid.}

\item{\code{columns}}{The number of columns in the grid.}

\item{\code{minX}}{The x-index of the grid's min-value.}

\item{\code{minY}}{The y-index of the grid's min-value.}

\item{\code{maxX}}{The x-index of the grid's max-value.}

\item{\code{maxY}}{The y-index of the grid's max-value.}

\item{\code{description}}{A description of the geo data grid.}

\item{\code{isLocked}}{This flag specifies whether the geo data grid is locked.
Defaults to \code{FALSE}.}

\item{\code{user}}{The user the geo data grid belongs to.}

\item{\code{isFavorite}}{This flag specifies whether the geo data grid is marked
as favorite item, and thus shown in the favorites tab on the home
module in PetroVisor. Defaults to \code{FALSE}.}

\item{\code{labels}}{A list of strings holding the labels of the geo data grid.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoDataGrid-toList"></a>}}
\if{latex}{\out{\hypertarget{method-GeoDataGrid-toList}{}}}
\subsection{Method \code{toList()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoDataGrid$toList()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Convert the object to a list. This function is mainly used
by the RepositoryService to convert the objects to lists and then
call the web API.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoDataGrid-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GeoDataGrid-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoDataGrid$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
