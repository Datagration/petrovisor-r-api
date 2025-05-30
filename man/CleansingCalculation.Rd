% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Calculation.R
\name{CleansingCalculation}
\alias{CleansingCalculation}
\title{CleansingCalculation}
\description{
Class representing a PetroVisor cleansing calculation object.
}
\examples{
\dontrun{
CleansingCalculation$new()
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{The name of the cleansing calculation.}

\item{\code{filters}}{A list of filters of the cleansing calculation. Each item has
to be an instance of the class \code{CleansingFilter}.}

\item{\code{isLocked}}{This flag specifies whether the cleansing calculation is
locked. Defaults to \code{FALSE}.}

\item{\code{user}}{The user the cleansing calculation belongs to.}

\item{\code{isFavorite}}{This flag specifies whether the cleansing calculation is
marked as favorite item, and thus shown in the favorites tab on the home
module in PetroVisor. Defaults to \code{FALSE}.}

\item{\code{labels}}{A list of strings holding the labels of the cleansing
calculation.}

\item{\code{formula}}{The cleansing calculation's formula as string.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CleansingCalculation-new}{\code{CleansingCalculation$new()}}
\item \href{#method-CleansingCalculation-toList}{\code{CleansingCalculation$toList()}}
\item \href{#method-CleansingCalculation-clone}{\code{CleansingCalculation$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CleansingCalculation-new"></a>}}
\if{latex}{\out{\hypertarget{method-CleansingCalculation-new}{}}}
\subsection{Method \code{new()}}{
Create a new CleansingCalculation instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CleansingCalculation$new(
  name = NULL,
  filters = NULL,
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
\item{\code{name}}{The name of the cleansing calculation.}

\item{\code{filters}}{A list of filters of the cleansing calculation. Each item has
to be an instance of the class \code{CleansingFilter}.}

\item{\code{isLocked}}{This flag specifies whether the cleansing calculation is
locked. Defaults to \code{FALSE}.}

\item{\code{user}}{The user the cleansing calculation belongs to.}

\item{\code{isFavorite}}{This flag specifies whether the cleansing calculation is
marked as favorite item, and thus shown in the favorites tab on the home
module in PetroVisor. Defaults to \code{FALSE}.}

\item{\code{labels}}{A list of strings holding the labels of the cleansing
calculation.}

\item{\code{formula}}{The cleansing calculation's formula as string.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CleansingCalculation-toList"></a>}}
\if{latex}{\out{\hypertarget{method-CleansingCalculation-toList}{}}}
\subsection{Method \code{toList()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CleansingCalculation$toList()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Convert the object to a list. This function is mainly used
by the RepositoryService to convert the objects to lists and then
call the web API.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CleansingCalculation-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CleansingCalculation-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CleansingCalculation$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
