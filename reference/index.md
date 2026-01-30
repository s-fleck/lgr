# Package index

## Logger

Creates and dispatches LogEvents

- [`Logger`](https://s-fleck.github.io/lgr/reference/Logger.md)
  [`Loggers`](https://s-fleck.github.io/lgr/reference/Logger.md) :
  Loggers
- [`LoggerGlue`](https://s-fleck.github.io/lgr/reference/LoggerGlue.md)
  : LoggerGlue
- [`print(`*`<Logger>`*`)`](https://s-fleck.github.io/lgr/reference/print.Logger.md)
  [`format(`*`<Logger>`*`)`](https://s-fleck.github.io/lgr/reference/print.Logger.md)
  [`print(`*`<ancestry>`*`)`](https://s-fleck.github.io/lgr/reference/print.Logger.md)
  [`format(`*`<ancestry>`*`)`](https://s-fleck.github.io/lgr/reference/print.Logger.md)
  : Print a Logger Object
- [`logger_tree()`](https://s-fleck.github.io/lgr/reference/logger_tree.md)
  : Logger Tree
- [`print(`*`<logger_tree>`*`)`](https://s-fleck.github.io/lgr/reference/print.logger_tree.md)
  [`format(`*`<logger_tree>`*`)`](https://s-fleck.github.io/lgr/reference/print.logger_tree.md)
  : Print Logger Trees
- [`logger_index()`](https://s-fleck.github.io/lgr/reference/logger_index.md)
  : Return a data.frame of all registered loggers
- [`get_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  [`add_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  [`remove_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  : Manage Log Levels
- [`log_exception()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  [`threshold()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  [`console_threshold()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  [`add_appender()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  [`remove_appender()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  [`show_log()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  [`show_dt()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  [`show_data()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  : Simple Logging
- [`basic_config()`](https://s-fleck.github.io/lgr/reference/basic_config.md)
  : Basic Setup for the Logging System
- [`get_logger()`](https://s-fleck.github.io/lgr/reference/get_logger.md)
  [`get_logger_glue()`](https://s-fleck.github.io/lgr/reference/get_logger.md)
  : Get/Create a Logger
- [`logger_config()`](https://s-fleck.github.io/lgr/reference/logger_config.md)
  [`as_logger_config()`](https://s-fleck.github.io/lgr/reference/logger_config.md)
  : Logger Configuration Objects
- [`default_exception_handler()`](https://s-fleck.github.io/lgr/reference/default_exception_handler.md)
  : Demote an exception to a warning

## Appenders

Output LogEvents to a destination

- [`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md)
  : Log to the console
- [`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
  [`AppenderJson`](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
  : Log to a file
- [`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md)
  : Log to a rotating file
- [`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md)
  : Log to a date-stamped rotating file
- [`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md)
  : Log to a time-stamped rotating file
- [`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md)
  : Log to a memory buffer
- [`print(`*`<Appender>`*`)`](https://s-fleck.github.io/lgr/reference/print.Appender.md)
  : Print an Appender object

## Layouts

Format LogEvents before output

- [`LayoutFormat`](https://s-fleck.github.io/lgr/reference/LayoutFormat.md)
  : Format Log Events as Text
- [`LayoutGlue`](https://s-fleck.github.io/lgr/reference/LayoutGlue.md)
  : Format Log Events as Text via glue
- [`LayoutJson`](https://s-fleck.github.io/lgr/reference/LayoutJson.md)
  : Format LogEvents as JSON

## LogEvent

Contains the Data that is beeing logged

- [`LogEvent`](https://s-fleck.github.io/lgr/reference/LogEvent.md)
  [`LogEvents`](https://s-fleck.github.io/lgr/reference/LogEvent.md) :
  LogEvents - The atomic unit of logging
- [`as_LogEvent()`](https://s-fleck.github.io/lgr/reference/as_LogEvent.md)
  : Coerce objects to LogEvent
- [`as.data.frame(`*`<LogEvent>`*`)`](https://s-fleck.github.io/lgr/reference/as.data.frame.LogEvent.md)
  [`as.data.table.LogEvent()`](https://s-fleck.github.io/lgr/reference/as.data.frame.LogEvent.md)
  [`as_tibble.LogEvent()`](https://s-fleck.github.io/lgr/reference/as.data.frame.LogEvent.md)
  : Coerce LogEvents to Data Frames
- [`event_list()`](https://s-fleck.github.io/lgr/reference/event_list.md)
  [`as_event_list()`](https://s-fleck.github.io/lgr/reference/event_list.md)
  [`as.data.table.event_list()`](https://s-fleck.github.io/lgr/reference/event_list.md)
  [`as.data.frame(`*`<event_list>`*`)`](https://s-fleck.github.io/lgr/reference/event_list.md)
  : A List of LogEvents
- [`print(`*`<LogEvent>`*`)`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md)
  [`format(`*`<LogEvent>`*`)`](https://s-fleck.github.io/lgr/reference/print.LogEvent.md)
  : Print or Format Logging Data
- [`toString(`*`<LogEvent>`*`)`](https://s-fleck.github.io/lgr/reference/toString.LogEvent.md)
  : Convert a LogEvent to a character string

## Filters

Filter events passed to Appenders or Loggers

- [`.obj()`](https://s-fleck.github.io/lgr/reference/EventFilter.md) :
  Event Filters
- [`FilterForceLevel`](https://s-fleck.github.io/lgr/reference/FilterForceLevel.md)
  : Override the log level of all events processed by a Logger/Appender
- [`FilterInject`](https://s-fleck.github.io/lgr/reference/FilterInject.md)
  : Inject values into all events processed by a Logger/Appender
- [`is_filter()`](https://s-fleck.github.io/lgr/reference/is_filter.md)
  : Check if an R Object is a Filter

## Utilities

General utility functions. Some may only be relevant to developers that
want to extend lgr.

- [`suspend_logging()`](https://s-fleck.github.io/lgr/reference/suspend_logging.md)
  [`unsuspend_logging()`](https://s-fleck.github.io/lgr/reference/suspend_logging.md)
  [`without_logging()`](https://s-fleck.github.io/lgr/reference/suspend_logging.md)
  [`with_logging()`](https://s-fleck.github.io/lgr/reference/suspend_logging.md)
  : Suspend All Logging
- [`standardize_threshold()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)
  [`is_threshold()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)
  [`standardize_log_level()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)
  [`is_log_level()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)
  [`standardize_log_levels()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)
  [`is_log_levels()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)
  : Standardize User-Input Log Levels to Their Integer Representation
- [`string_repr()`](https://s-fleck.github.io/lgr/reference/string_repr.md)
  : Short string representation for R objects
- [`with_log_level()`](https://s-fleck.github.io/lgr/reference/with_log_level.md)
  [`with_log_value()`](https://s-fleck.github.io/lgr/reference/with_log_level.md)
  : Inject Values into Logging Calls
- [`get_caller()`](https://s-fleck.github.io/lgr/reference/system_infos.md)
  [`get_user()`](https://s-fleck.github.io/lgr/reference/system_infos.md)
  : Information About the System
- [`read_json_lines()`](https://s-fleck.github.io/lgr/reference/read_json_lines.md)
  : Read a JSON logfile
- [`use_logger()`](https://s-fleck.github.io/lgr/reference/use_logger.md)
  : Setup a Simple Logger for a Package
- [`label_levels()`](https://s-fleck.github.io/lgr/reference/label_levels.md)
  [`unlabel_levels()`](https://s-fleck.github.io/lgr/reference/label_levels.md)
  : Label/Unlabel Log Levels
- [`colorize_levels()`](https://s-fleck.github.io/lgr/reference/colorize_levels.md)
  : Colorize Levels
- [`pad_right`](https://s-fleck.github.io/lgr/reference/pad_right.md)
  [`pad_left`](https://s-fleck.github.io/lgr/reference/pad_right.md) :
  Pad Character Vectors

## Abstract Classes

For Package developers that want to build new Appenders, Layouts or
Loggers

- [`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
  [`Appenders`](https://s-fleck.github.io/lgr/reference/Appender.md) :
  Appenders
- [`Layout`](https://s-fleck.github.io/lgr/reference/Layout.md)
  [`Layouts`](https://s-fleck.github.io/lgr/reference/Layout.md) :
  Abstract Class for Layouts
- [`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
  : Abstract Class for Filterables
- [`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)
  : Abstract class for logging to tabular structures
- [`AppenderMemory`](https://s-fleck.github.io/lgr/reference/AppenderMemory.md)
  : Abstract class for logging to memory buffers

- [`CannotInitializeAbstractClassError()`](https://s-fleck.github.io/lgr/reference/CannotInitializeAbstractClassError.md)
  : Logger Error Conditions
