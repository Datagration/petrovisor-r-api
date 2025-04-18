% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataTypes.R
\name{Point}
\alias{Point}
\title{Point}
\description{
Class representing a point with x- and y-value.
}
\examples{
\dontrun{
Point$new(x = 10, y = 20)
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{x}}{The x-value of the point.}

\item{\code{y}}{The y-value of the point.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Point-new}{\code{Point$new()}}
\item \href{#method-Point-toList}{\code{Point$toList()}}
\item \href{#method-Point-clone}{\code{Point$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Point-new"></a>}}
\if{latex}{\out{\hypertarget{method-Point-new}{}}}
\subsection{Method \code{new()}}{
Create a new Point instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Point$new(x = NULL, y = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{The x-value of the point.}

\item{\code{y}}{The y-value of the point.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Point-toList"></a>}}
\if{latex}{\out{\hypertarget{method-Point-toList}{}}}
\subsection{Method \code{toList()}}{
Convert the object to a list. This function is mainly used
by the Services to convert the objects to lists and then
call the web API.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Point$toList()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Point-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Point-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Point$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
