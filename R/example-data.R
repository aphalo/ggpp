#' @title Gene expression data
#'
#' @description A dataset containing reshaped and simplified output from an
#'   analysis of data from RNAseq done with package edgeR. Original data from
#'   gene expression in the plant species \emph{Arabidopsis thaliana}.
#'
#' @docType data
#' @keywords datasets
#' @format A \code{data.frame} object with 1218 rows and 5 variables
#' @family Transcriptomics data
#' @references Rai, Neha; O'Hara, Andrew; Farkas, Daniel; Safronov, Omid;
#' Ratanasopa, Khuanpiroon; Wang, Fang; Lindfors, Anders V.; Jenkins,
#' Gareth I.; Lehto, Tarja; Salojärvi, Jarkko; Brosché, Mikael; Strid. Åke;
#'  Aphalo, Pedro José; Morales, Luis Orlando (2020) The photoreceptor UVR8
#'  mediates the perception of both UV-B and UV-A wavelengths up to 350 nm of
#'  sunlight with responsivity moderated by cryptochromes. \emph{Plant, Cell &
#'  Environment}, 43:1513-1527.
#'
#' @examples
#' colnames(volcano_example.df)
#' head(volcano_example.df)
#'
"volcano_example.df"

#' @title Gene expression data
#'
#' @description A dataset containing reshaped and simplified output from an
#'   analysis of data from RNAseq done with package edgeR. Original data from
#'   gene expression in the plant species \emph{Arabidopsis thaliana}.
#'
#' @docType data
#' @keywords datasets
#' @format A \code{data.frame} object with 6088 rows and 6 variables
#' @family Transcriptomics data
#' @references Rai, Neha; O'Hara, Andrew; Farkas, Daniel; Safronov, Omid;
#' Ratanasopa, Khuanpiroon; Wang, Fang; Lindfors, Anders V.; Jenkins,
#' Gareth I.; Lehto, Tarja; Salojärvi, Jarkko; Brosché, Mikael; Strid. Åke;
#'  Aphalo, Pedro José; Morales, Luis Orlando (2020) The photoreceptor UVR8
#'  mediates the perception of both UV-B and UV-A wavelengths up to 350 nm of
#'  sunlight with responsivity moderated by cryptochromes. \emph{Plant, Cell &
#'  Environment}, 43:1513-1527.
#'
#' @examples
#' colnames(quadrant_example.df)
#' head(quadrant_example.df)
#'
"quadrant_example.df"

#' @title Birch seedlings' size
#'
#' @description A dataset containing the measurements on 350 birch seedlings.
#'
#' @details The data are for seedlings grown in trays with cells or containers
#' of two different volumes. For each of these types of trays, all cells, 1/2 of
#' the cells or 1/4 of the cells contained seedlings. Root-collar diameter (mm),
#' height (cm), dry mass (mg) of stems and roots. Measurements done at the end
#' of the first growing season, after leaf fall.
#'
#' @references Aphalo, P. J. and Rikala, R. (2003) Field performance of
#' silver-birch planting-stock grown at different spacing and in containers of
#' different volume. \emph{New Forests}, 25:93-108. \doi{10.1023/A:1022618810937}.
#'
#' @docType data
#' @keywords datasets
#' @format A \code{data.frame} object with 350 rows and 8 variables.
#' @family Plant growth and morphology data
#'
#' @examples
#' colnames(birch.df)
#' head(birch.df)
#'
#' colnames(birch_dw.df)
#' head(birch_dw.df)
#'
"birch.df"

#' @rdname birch.df
#' @format A \code{data.frame} object with 700 rows and 5 variables.
#'
"birch_dw.df"
