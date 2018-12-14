% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Logger.R
\name{Logger}
\alias{Logger}
\alias{Loggers}
\title{Loggers}
\description{
A Logger records the log message and some metadata (timestamp,
calling function) as a \link{LogEvent} and passes this event on to one or
several \link{Appenders} that write the event to a destination (a file,
the console, ...). All Loggers of an R session are in a hierarchical
structure, and each Logger (except the Root Looger) passes on LogEvents to
the Appenders of it ancestral Loggers. See \code{vignette("yog", package = "yog")}
for more info.
}
\section{Usage}{
\preformatted{x <- Logger$new(name, appenders = list(), threshold = 400L, user = get_user(),
  parent = yog::yog, exception_handler = function(e) { warning("[",
  format(Sys.time(), format = "\%Y-\%m-\%d \%H:\%M:\%OS3"), "] ", "An error occured
  in the logging sub system: ", e) }, propagate = TRUE)

x$log(level, msg, timestamp = Sys.time(), caller = get_caller(-3))
x$fatal(msg, ...)
x$error(msg, ...)
x$warn(msg, ...)
x$info(msg, ...)
x$debug(msg, ...)
x$trace(msg, ...)
x$add_appender(appender, name = NULL)
x$remove_appender(pos)
x$handle_exception(...)
x$set_name(x)
x$set_exception_handler(fun)
x$set_propagate(x)
x$set_parent(logger)
x$set_threshold(level)
x$set_appenders(x)
x$set_user(x)

x$ancestry
x$appenders
x$exception_handler
x$inherited_appenders
x$last_event
x$name
x$parent
x$propagate
x$threshold
x$user
}
}

\section{Creating Loggers}{


If you want logging for a Project (f.e a Package you are developing) that is
separate from the global logging, you can create a new logger with
\code{Logger$new()}. If you just want to add different outputs (for example
logfiles) to the Root Logger, look into \link{Appenders}.
}

\section{Fields}{


You can either specify the fields in \code{Logger$new()} or modify them after
creation with setter functions of the form \code{logger$set_fieldname(value)}
(see examples)

\describe{
\item{\code{name}}{\code{character} scalar. Name of the Logger. Should be unique amongst
Loggers. If you define a logger for an R Package, the logger should have
the same name as the Package.}

\item{\code{appenders}}{\code{list} of \link{Appender}s. The appenders used by this logger
to write log entries to the console, to files, etc...}

\item{\code{threshold}}{\code{character} or \code{integer} scalar. The minimum log level
that triggers this logger}

\item{\code{user}}{\code{character} scalar. The current user name or email adress.
This information can be used by the appenders}

\item{\code{parent}}{a \code{Logger}. Usually the Root logger. All Loggers must be
descentents of the Root logger for yog to work as intended.}

\item{\code{exception_handler}}{a \code{function} that takes a single argument \code{e}.
The function used to handle errors that occur durring loging. Default
to demoting any error to a \link{warning}}

\item{\code{propagate}}{\code{TRUE} or \code{FALSE}. Should log messages be passed on to
the appenders of the ancestral Loggers?}
}
}

\section{Methods}{


\describe{
\item{\code{fatal(msg, ...)}}{Logs a message with level \code{FATAL} on this logger.
The arguments are interpreted as for \code{trace()}.}

\item{\code{error(msg, ...)}}{Logs a message with level \code{ERROR} on this logger.
The arguments are interpreted as for \code{trace()}.}

\item{\code{warn(msg, ...)}}{Logs a message with level \code{WARN} on this logger.
The arguments are interpreted as for \code{trace()}.}

\item{\code{info(msg, ...)}}{Logs a message with level \code{INFO} on this logger.
The arguments are interpreted as for \code{trace()}.}

\item{\code{debug(msg, ...)}}{Logs a message with level \code{DEBUG} on this logger.
The arguments are interpreted as for \code{trace()}.}

\item{\code{trace(msg, ...)}}{Logs a message with level \code{TRACE} on this logger.
\code{msg} and \code{...} are passed on to \code{\link[base:sprintf]{base::sprintf()}}.}

\item{\code{log(level, msg, timestamp, caller)}}{Logs a message with \code{level}.}

\item{\code{add_appender(appender, name = NULL)}}{Adds a new Appender to the
Logger. \code{appender} must be an \link{Appender} object. \code{name} is optional and
will be used as name in the list of appenders, i.e if you do
\code{logger$add_appender(AppenderConsole$new(), name = "console")} you can
refer to it via \code{logger$appenders$console}.}

\item{\code{remove_appender(pos)}}{Removes and Appender from a Logger. \code{pos}
can be an \code{integer} or \code{character} vector referring either to the positions
or names of the Appenders to be removed.}

\item{\code{exception_handler(e)}}{Used to handle errors that occur durring the
logging process. The defaul is to convert errors to warnings.}

\item{\code{filter(x)}}{Determine whether the LogEvent \code{x} should be passed
on to Appenders (\code{TRUE}) or not (\code{FALSE}). See also the active binding
\code{filters}}

\item{\code{set_name(x)}}{Set the Logger name to the \code{character} scalar \code{x}}

}
}

\section{Active Bindings}{


\describe{

\item{\code{appenders}}{A \code{list} of all Appenders attached to the current
logger}

\item{\code{ancestry}}{\emph{read only}. A \code{character} vector of the names of all
Loggers that are ancestors to the current Logger}

\item{\code{inherited_appenders}}{\emph{read only}. A \code{list} of all inherited
appenders from ancestral Loggers of the current Logger}

\item{\code{filters}}{a \code{list} of predicates (functions that return either
\code{TRUE} or \code{FALSE}). If all of these functions evaluate to \code{TRUE} the
LogEvent is passed on to the Logger's Appenders}

\item{\code{last_event}}{The last LogEvent produced by the current Logger}

\item{\code{name}}{Name of the Logger. Mainly used for display purposes. If
you define a Logger for a package, this should be the same name as
the packages
}

\item{\code{parent}}{Parent Logger of the current Logger (\code{NULL} for the Root Logger)}

\item{\code{threshold}}{An \code{integer}. Threshold of the current Logger (i.e the
maximum log level that this Logger processes)}

\item{\code{user}}{The current user}
}
}

\examples{

yog$info("Today is \%s", Sys.Date() )

# yog includes a pre-configured root logger
yog$fatal("This is a serious error")

# if you want to take advantage of hierarchical logging, you can create new loggers.
# the following creates a new logger that logs to a temporary file.
tf <- tempfile()
mylogger <- Logger$new(
  "mylogger",
  appenders = AppenderFile$new(tf)
)

# The new logger passes the log message on to the appenders of its parent
# logger, which is by default the root logger. This is why the following
# writes not only the file 'tf', but also to the console.
mylogger$fatal("blubb")
readLines(tf)

}