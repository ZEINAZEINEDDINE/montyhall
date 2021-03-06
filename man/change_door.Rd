% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newscriptmontyhall.R
\name{change_door}
\alias{change_door}
\title{Change Doors.}
\usage{
change_door(stay = T, opened.door, a.pick)
}
\arguments{
\item{...}{a logical vector whether the contestant chooses to stay or change his initial selection.}
}
\value{
The function returns a door number which is between one and three.
}
\description{
change_door()generates a number of the door that the contestant choses if he made a decision to switch doors.
}
\details{
The contestant is given the option to change from their initial selection to the other door that is still closed.
}
\examples{
change_door()

}
