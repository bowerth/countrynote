#' exgrdvaex
#'
#' exgrdvaex indicator
#'
#' Use exgrdvaex indicator.
#'
#' @param data the input data to use.
#' @param cou character ISO3 country code.
#' @param yr integer latest year. 
#' @param refyr integer reference year.
#'
#' @author Bo Werth
#' @importFrom magrittr %>%
#' @export
#' @examples
#' data(icioIndic_data, package = "countrynote")
#' exgrdvaex(icioIndic_data, "USA", 2009, 1995)

exgrdvaex <- function(data, cou, yr, refyr) {

  icioindic_chart <- "exgrdvaex"
  nameindic <- ui.icioIndic.indic[[icioindic_chart]]

  data_copy <-
    data %>%
    subset(indic%in%nameindic & ind=="Total")

  data_year <-
    fun_exgrdvaex(data = subset(data_copy, year==yr), cou = cou)

  data_refyear <-
    fun_exgrdvaex(data = subset(data_copy, year==refyr), cou)

  avrg <- round(mean(data_year$value[data_year$cou%in%icioIndic_reg[["oecd"]]]))

  data_plot <-
    list(data_year = data_year,
         data_refyear = data_refyear)

  data_summary <-
    list(title = paste('Domestic value added content of gross exports,', yr),
         avrg = avrg)

  return(list(data_plot = data_plot,
              data_summary = data_summary))
}


#' @export

exgrdvaexPlot <- function(data_plot, data_summary) {
  p <-
    ggplot(data = data_plot[["data_year"]],
           aes(x = cou, y = value, fill = as.factor(col))) +
    geom_bar(stat = "identity", width = 0.4,
             position = position_dodge(width=0.5)) +
    geom_point(data = data_plot[["data_refyear"]],
               aes(x = cou, y = value, color = as.factor(col)),
               fill = "white", size = 3, shape = 23) +
    geom_hline(yintercept = data_summary[["avrg"]],
               color = "grey50", size = 0.5, linetype = 2) +
    scale_fill_brewer(guide = "none", palette = "Set1") +
    scale_color_brewer(guide = "none", palette = "Set1") +
    ggtitle(data_summary[["title"]])
  return(p)
}


fun_exgrdvaex <- function(data,
                          cou) {
  data_copy <- data
  data_copy$cou <- factor(data_copy$cou,
                          levels = data_copy$cou[order(data_copy$value)])
  data_copy$col <- 1
  data_copy$col[data_copy$cou==cou] <- 0
  return(data_copy)
}
