% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Filter.R
\name{EventFilter}
\alias{EventFilter}
\alias{Filter}
\alias{.obj}
\title{Event Filters}
\usage{
.obj()
}
\description{
EventFilters specify arbitrarily complex logic for whether or
not a LogEvent should be processed by a \link{Logger} or \link{Appender}. They are
attached to Loggers/Appenders via their \verb{$set_filter()} or \verb{$add_filter()}
methods. If any EventFilter evaluates to \code{FALSE} for a given event, that
event is ignored - similarly to when it does not pass the objects'
threshold.

Usually you do not need to instantiate a formal \code{EventFilter} object as you
can just use any \code{function} that has the single argument \code{event} instead.
If you need to implement more complex filter logic - for example a filter
that is dependent on a dataset - it might be desirable to subclass
EventFilter, as \link[R6:R6Class]{R6::R6} objects can store data and functions together.

\code{.obj()} is a special function that can only be used within the
\verb{$filter()} methods of \link[=EventFilter]{EventFilters}. It returns the \link{Logger}
or \link{Appender} that the EventFilter is attached to.
}
\section{Modifying LogEvents with EventFilters}{


Since LogEvents are R6 objects with reference semantics, EventFilters can be
abused to modify events before passing them on. lgr comes with a few
preset filters that use this property: \link{FilterInject} (similar to
\code{\link[=with_log_level]{with_log_level()}}) and \link{FilterForceLevel} (similar to \code{\link[=with_log_value]{with_log_value()}}).

\strong{NOTE:} The base class for Filters is called \code{EventFilter} so that it
doesn't conflict with \code{\link[base:funprog]{base::Filter()}}. The recommended convention for
Filter subclasses is to call them \code{FilterSomething} and leave out the
\code{Event} prefix.
}

\examples{
lg <- get_logger("test")
f <- function(event) {
  cat("via event$.logger:", event$.logger$threshold, "\n")  #  works for loggers only
  cat("via .obj():      ",.obj()$threshold, "\n") # works for loggers and appenders
  TRUE
}
lg$add_filter(f)
lg$fatal("test")
lg$config(NULL)
}
\seealso{
\code{\link[=is_filter]{is_filter()}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-EventFilter-new}{\code{EventFilter$new()}}
\item \href{#method-EventFilter-clone}{\code{EventFilter$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-EventFilter-new"></a>}}
\if{latex}{\out{\hypertarget{method-EventFilter-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new EventFilter
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{EventFilter$new(fun = function(event) TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fun}}{a \code{function} with a single argument \code{event} that must return
either \code{TRUE} or \code{FALSE}. Any  non-\code{FALSE} will be interpreted as
\code{TRUE} (= no filtering takes place) and a warning will be thrown.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-EventFilter-clone"></a>}}
\if{latex}{\out{\hypertarget{method-EventFilter-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{EventFilter$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
