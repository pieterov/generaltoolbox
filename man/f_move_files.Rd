% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f - Move files.R
\name{f_move_files}
\alias{f_move_files}
\title{Move (or copy) files between folders}
\usage{
f_move_files(
  c.path.source,
  c.path.destination,
  v.file.to.move = NULL,
  c.date.treshold = NULL,
  b.delete.from.source = TRUE,
  b.overwrite = TRUE,
  b.save.file.list = FALSE
)
}
\arguments{
\item{c.path.source}{Full path to folder that holds files to be moved/copied.}

\item{c.path.destination}{Full path to folder where files should be moved/copied to. In case it does not exist,
it will be created. Though, the parent of the new folder must exist, else an error will be thrown.}

\item{v.file.to.move}{List of files in c.path.source to be moved/copied. Each value should be the filename plus its
extension. By default the value is NULL, resulting in the files to be moved/copied.}

\item{c.date.treshold}{In case given - e.g., 'today() - 60' - files older than this date will be moved/copied. The date
is derived from what is given in the filename. By default the value is NULL, resulting in the files to be moved/copied.}

\item{b.delete.from.source}{Should we move or copy the files? By default the value is TRUE, effectively moving the
files, and when set to FALSE, effectively copying the files.}

\item{b.overwrite}{Should we overwrite files in case they are already present in the destination folder? By default
the value is TRUE. In case, the value is set to FALSE, files will not be overwritten, if applicable.}

\item{b.save.file.list}{Should we save the list of moved/copied files to an Excel file? The default is set to FALSE.}
}
\value{
A data frame listing the moved/copied files.
}
\description{
This function can be used to move (or copy) files from one folder (source) to another folder (destination).
}
\details{
In case files from the source folder already occur in the destination folder, they will not be removed from
the source folder when b.overwrite has been set to FALSE and b.delete.from.source has been set to TRUE. This is to
prevent loosing the concerned files.
}
\examples{
df.move <- f_move_files(c.path.source = "...", c.path.destination = "...")
}
\author{
Pieter Overdevest
}
