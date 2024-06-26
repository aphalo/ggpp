% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-continuous-npc.r
\name{scale_continuous_npc}
\alias{scale_continuous_npc}
\alias{scale_npcx_continuous}
\alias{scale_npcy_continuous}
\title{Position scales for continuous data (npcx & npcy)}
\usage{
scale_npcx_continuous(...)

scale_npcy_continuous(...)
}
\arguments{
\item{...}{Other arguments passed on to \code{continuous_scale()}}
}
\value{
A \code{"Scale"} object.
}
\description{
\code{scale_npcx_continuous()} and \code{scale_npcy_continuous()} are
scales for continuous npcx and npcy aesthetics expressed in "npc" units.
There are no variants. Obviously limits are always the full range of "npc"
units and transformations meaningless. These scales are used by the newly
defined aesthetics \code{npcx} and \code{npcy}.
}
