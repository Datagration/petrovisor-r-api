% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProcessTemplate.R
\name{Step}
\alias{Step}
\title{Step}
\description{
Class representing a process template step.
}
\examples{
\dontrun{
Step$new(name = "DoThis", userGroupName = "Engineers")
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{The name of the step.}

\item{\code{userGroupName}}{The name of the user group assigned to the step.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Step-new}{\code{Step$new()}}
\item \href{#method-Step-toList}{\code{Step$toList()}}
\item \href{#method-Step-clone}{\code{Step$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Step-new"></a>}}
\if{latex}{\out{\hypertarget{method-Step-new}{}}}
\subsection{Method \code{new()}}{
Create a new Step instance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Step$new(name = NULL, userGroupName = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the step.}

\item{\code{userGroupName}}{The name of the user group assigned to the step.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Step-toList"></a>}}
\if{latex}{\out{\hypertarget{method-Step-toList}{}}}
\subsection{Method \code{toList()}}{
Convert the object to a list. This function is mainly used
by the Services to convert the objects to lists and then
call the web API.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Step$toList()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Step-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Step-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Step$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
