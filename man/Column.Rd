% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Calculation.R
\name{Column}
\alias{Column}
\title{Column}
\description{
Class representing a column object.
}
\examples{
\dontrun{
Column$new()
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{The name of the column.}

\item{\code{unit}}{The unit of the column. Has to be an object of the class
\code{Unit}.}

\item{\code{formula}}{The columns's formula.}

\item{\code{isStatic}}{This flag specifies whether the column is static or
time-dependent.}

\item{\code{savingSignalName}}{The name of the signal in the columns saving clause.}

\item{\code{saveToParentEntity}}{This flag specifies whether data shall be saved to
the parent of the entity.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Column-new}{\code{Column$new()}}
\item \href{#method-Column-toList}{\code{Column$toList()}}
\item \href{#method-Column-clone}{\code{Column$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Column-new"></a>}}
\if{latex}{\out{\hypertarget{method-Column-new}{}}}
\subsection{Method \code{new()}}{
Create a new Column instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Column$new(
  name = NULL,
  unit = NULL,
  formula = NULL,
  isStatic = NULL,
  savingSignalName = NULL,
  saveToParentEntity = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the column.}

\item{\code{unit}}{The unit of the column. Has to be an object of the class
\code{Unit}.}

\item{\code{formula}}{The columns's formula.}

\item{\code{isStatic}}{This flag specifies whether the column is static or
time-dependent.}

\item{\code{savingSignalName}}{The name of the signal in the columns saving
clause.}

\item{\code{saveToParentEntity}}{This flag specifies whether data shall be saved
to the parent of the entity.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Column-toList"></a>}}
\if{latex}{\out{\hypertarget{method-Column-toList}{}}}
\subsection{Method \code{toList()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Column$toList()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Convert the object to a list. This function is mainly used
by the RepositoryService to convert the objects to lists and then
call the web API.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Column-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Column-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Column$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
