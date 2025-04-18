% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Calculation.R
\name{TableCalculation}
\alias{TableCalculation}
\title{TableCalculation}
\description{
Class representing a PetroVisor table calculation object.
}
\examples{
\dontrun{
TableCalculation$new()
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{The name of the table calculation.}

\item{\code{columns}}{A list of columns of the table calculation. Each item has
to be an instance of the class \code{Column}.}

\item{\code{inputTableNames}}{A list containing the names of all input tables for
the table calculation.}

\item{\code{isLocked}}{This flag specifies whether the table calculation is
locked. Defaults to \code{FALSE}.}

\item{\code{user}}{The user the table calculation belongs to.}

\item{\code{isFavorite}}{This flag specifies whether the table calculation is
marked as favorite item, and thus shown in the favorites tab on the home
module in PetroVisor. Defaults to \code{FALSE}.}

\item{\code{labels}}{A list of strings holding the labels of the table
calculation.}

\item{\code{formula}}{The table calculation's formula as string.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TableCalculation-new}{\code{TableCalculation$new()}}
\item \href{#method-TableCalculation-toList}{\code{TableCalculation$toList()}}
\item \href{#method-TableCalculation-clone}{\code{TableCalculation$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableCalculation-new"></a>}}
\if{latex}{\out{\hypertarget{method-TableCalculation-new}{}}}
\subsection{Method \code{new()}}{
Create a new EventCalculation instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableCalculation$new(
  name = NULL,
  columns = NULL,
  inputTableNames = NULL,
  isLocked = FALSE,
  user = NULL,
  isFavorite = FALSE,
  labels = NULL,
  formula = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the table calculation.}

\item{\code{columns}}{A list of columns of the table calculation. Each item has
to be an instance of the class \code{Column}.}

\item{\code{inputTableNames}}{A list containing the names of all input tables for
the table calculation.}

\item{\code{isLocked}}{This flag specifies whether the table calculation is
locked. Defaults to \code{FALSE}.}

\item{\code{user}}{The user the table calculation belongs to.}

\item{\code{isFavorite}}{This flag specifies whether the table calculation is
marked as favorite item, and thus shown in the favorites tab on the home
module in PetroVisor. Defaults to \code{FALSE}.}

\item{\code{labels}}{A list of strings holding the labels of the table
calculation.}

\item{\code{formula}}{The table calculation's formula as string.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableCalculation-toList"></a>}}
\if{latex}{\out{\hypertarget{method-TableCalculation-toList}{}}}
\subsection{Method \code{toList()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableCalculation$toList()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Convert the object to a list. This function is mainly used
by the RepositoryService to convert the objects to lists and then
call the web API.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TableCalculation-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TableCalculation-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TableCalculation$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
