% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cplot.R, R/cplot_clm.R, R/cplot_glm.R,
%   R/cplot_lm.R, R/cplot_loess.R, R/cplot_polr.R
\name{cplot}
\alias{cplot}
\alias{cplot.default}
\alias{cplot.clm}
\alias{cplot.glm}
\alias{cplot.lm}
\alias{cplot.loess}
\alias{cplot.polr}
\alias{cplot.multinom}
\title{Conditional predicted value and average marginal effect plots for models}
\usage{
cplot(object, ...)

\method{cplot}{default}(
  object,
  x = attributes(terms(object))[["term.labels"]][1L],
  dx = x,
  what = c("prediction", "effect"),
  data = prediction::find_data(object),
  type = c("response", "link"),
  vcov = stats::vcov(object),
  at,
  n = 25L,
  xvals = prediction::seq_range(data[[x]], n = n),
  level = 0.95,
  draw = TRUE,
  xlab = x,
  ylab = if (match.arg(what) == "prediction") paste0("Predicted value") else
    paste0("Marginal effect of ", dx),
  xlim = NULL,
  ylim = NULL,
  lwd = 1L,
  col = "black",
  lty = 1L,
  se.type = c("shade", "lines", "none"),
  se.col = "black",
  se.fill = grDevices::gray(0.5, 0.5),
  se.lwd = lwd,
  se.lty = if (match.arg(se.type) == "lines") 1L else 0L,
  factor.lty = 0L,
  factor.pch = 19L,
  factor.col = se.col,
  factor.fill = factor.col,
  factor.cex = 1L,
  xaxs = "i",
  yaxs = xaxs,
  las = 1L,
  scatter = FALSE,
  scatter.pch = 19L,
  scatter.col = se.col,
  scatter.bg = scatter.col,
  scatter.cex = 0.5,
  rug = TRUE,
  rug.col = col,
  rug.size = -0.02,
  ...
)

\method{cplot}{clm}(
  object,
  x = attributes(terms(object))[["term.labels"]][1L],
  dx = x,
  what = c("prediction", "classprediction", "stackedprediction", "effect"),
  data = prediction::find_data(object),
  type = c("response", "link"),
  vcov = stats::vcov(object),
  at,
  n = 25L,
  xvals = seq_range(data[[x]], n = n),
  level = 0.95,
  draw = TRUE,
  xlab = x,
  ylab = if (match.arg(what) == "effect") paste0("Marginal effect of ", dx) else
    paste0("Predicted value"),
  xlim = NULL,
  ylim = if (match.arg(what) \%in\% c("prediction", "stackedprediction")) c(0, 1.04) else
    NULL,
  lwd = 1L,
  col = "black",
  lty = 1L,
  factor.lty = 1L,
  factor.pch = 19L,
  factor.col = col,
  factor.fill = factor.col,
  factor.cex = 1L,
  xaxs = "i",
  yaxs = xaxs,
  las = 1L,
  scatter = FALSE,
  scatter.pch = 19L,
  scatter.col = factor.col,
  scatter.bg = scatter.col,
  scatter.cex = 0.5,
  rug = TRUE,
  rug.col = col,
  rug.size = -0.02,
  ...
)

\method{cplot}{glm}(
  object,
  x = attributes(terms(object))[["term.labels"]][1L],
  dx = x,
  what = c("prediction", "effect"),
  data = prediction::find_data(object),
  type = c("response", "link"),
  vcov = stats::vcov(object),
  at,
  n = 25L,
  xvals = prediction::seq_range(data[[x]], n = n),
  level = 0.95,
  draw = TRUE,
  xlab = x,
  ylab = if (match.arg(what) == "prediction") paste0("Predicted value") else
    paste0("Marginal effect of ", dx),
  xlim = NULL,
  ylim = NULL,
  lwd = 1L,
  col = "black",
  lty = 1L,
  se.type = c("shade", "lines", "none"),
  se.col = "black",
  se.fill = grDevices::gray(0.5, 0.5),
  se.lwd = lwd,
  se.lty = if (match.arg(se.type) == "lines") 1L else 0L,
  factor.lty = 0L,
  factor.pch = 19L,
  factor.col = se.col,
  factor.fill = factor.col,
  factor.cex = 1L,
  xaxs = "i",
  yaxs = xaxs,
  las = 1L,
  scatter = FALSE,
  scatter.pch = 19L,
  scatter.col = se.col,
  scatter.bg = scatter.col,
  scatter.cex = 0.5,
  rug = TRUE,
  rug.col = col,
  rug.size = -0.02,
  ...
)

\method{cplot}{lm}(
  object,
  x = attributes(terms(object))[["term.labels"]][1L],
  dx = x,
  what = c("prediction", "effect"),
  data = prediction::find_data(object),
  type = c("response", "link"),
  vcov = stats::vcov(object),
  at,
  n = 25L,
  xvals = prediction::seq_range(data[[x]], n = n),
  level = 0.95,
  draw = TRUE,
  xlab = x,
  ylab = if (match.arg(what) == "prediction") paste0("Predicted value") else
    paste0("Marginal effect of ", dx),
  xlim = NULL,
  ylim = NULL,
  lwd = 1L,
  col = "black",
  lty = 1L,
  se.type = c("shade", "lines", "none"),
  se.col = "black",
  se.fill = grDevices::gray(0.5, 0.5),
  se.lwd = lwd,
  se.lty = if (match.arg(se.type) == "lines") 1L else 0L,
  factor.lty = 0L,
  factor.pch = 19L,
  factor.col = se.col,
  factor.fill = factor.col,
  factor.cex = 1L,
  xaxs = "i",
  yaxs = xaxs,
  las = 1L,
  scatter = FALSE,
  scatter.pch = 19L,
  scatter.col = se.col,
  scatter.bg = scatter.col,
  scatter.cex = 0.5,
  rug = TRUE,
  rug.col = col,
  rug.size = -0.02,
  ...
)

\method{cplot}{loess}(
  object,
  x = attributes(terms(object))[["term.labels"]][1L],
  dx = x,
  what = c("prediction", "effect"),
  data = prediction::find_data(object),
  type = c("response", "link"),
  vcov = stats::vcov(object),
  at,
  n = 25L,
  xvals = prediction::seq_range(data[[x]], n = n),
  level = 0.95,
  draw = TRUE,
  xlab = x,
  ylab = if (match.arg(what) == "prediction") paste0("Predicted value") else
    paste0("Marginal effect of ", dx),
  xlim = NULL,
  ylim = NULL,
  lwd = 1L,
  col = "black",
  lty = 1L,
  se.type = c("shade", "lines", "none"),
  se.col = "black",
  se.fill = grDevices::gray(0.5, 0.5),
  se.lwd = lwd,
  se.lty = if (match.arg(se.type) == "lines") 1L else 0L,
  factor.lty = 0L,
  factor.pch = 19L,
  factor.col = se.col,
  factor.fill = factor.col,
  factor.cex = 1L,
  xaxs = "i",
  yaxs = xaxs,
  las = 1L,
  scatter = FALSE,
  scatter.pch = 19L,
  scatter.col = se.col,
  scatter.bg = scatter.col,
  scatter.cex = 0.5,
  rug = TRUE,
  rug.col = col,
  rug.size = -0.02,
  ...
)

\method{cplot}{polr}(
  object,
  x = attributes(terms(object))[["term.labels"]][1L],
  dx = x,
  what = c("prediction", "classprediction", "stackedprediction", "effect"),
  data = prediction::find_data(object),
  type = c("response", "link"),
  vcov = stats::vcov(object),
  at,
  n = 25L,
  xvals = seq_range(data[[x]], n = n),
  level = 0.95,
  draw = TRUE,
  xlab = x,
  ylab = if (match.arg(what) == "effect") paste0("Marginal effect of ", dx) else
    paste0("Predicted value"),
  xlim = NULL,
  ylim = if (match.arg(what) \%in\% c("prediction", "stackedprediction")) c(0, 1.04) else
    NULL,
  lwd = 1L,
  col = "black",
  lty = 1L,
  factor.lty = 1L,
  factor.pch = 19L,
  factor.col = col,
  factor.fill = factor.col,
  factor.cex = 1L,
  xaxs = "i",
  yaxs = xaxs,
  las = 1L,
  scatter = FALSE,
  scatter.pch = 19L,
  scatter.col = factor.col,
  scatter.bg = scatter.col,
  scatter.cex = 0.5,
  rug = TRUE,
  rug.col = col,
  rug.size = -0.02,
  ...
)

\method{cplot}{multinom}(
  object,
  x = attributes(terms(object))[["term.labels"]][1L],
  dx = x,
  what = c("prediction", "classprediction", "stackedprediction", "effect"),
  data = prediction::find_data(object),
  type = c("response", "link"),
  vcov = stats::vcov(object),
  at,
  n = 25L,
  xvals = seq_range(data[[x]], n = n),
  level = 0.95,
  draw = TRUE,
  xlab = x,
  ylab = if (match.arg(what) == "effect") paste0("Marginal effect of ", dx) else
    paste0("Predicted value"),
  xlim = NULL,
  ylim = if (match.arg(what) \%in\% c("prediction", "stackedprediction")) c(0, 1.04) else
    NULL,
  lwd = 1L,
  col = "black",
  lty = 1L,
  factor.lty = 1L,
  factor.pch = 19L,
  factor.col = col,
  factor.fill = factor.col,
  factor.cex = 1L,
  xaxs = "i",
  yaxs = xaxs,
  las = 1L,
  scatter = FALSE,
  scatter.pch = 19L,
  scatter.col = factor.col,
  scatter.bg = scatter.col,
  scatter.cex = 0.5,
  rug = TRUE,
  rug.col = col,
  rug.size = -0.02,
  ...
)
}
\arguments{
\item{object}{A model object.}

\item{\dots}{Additional arguments passed to \code{\link[graphics]{plot}}.}

\item{x}{A character string specifying the name of variable to use as the x-axis dimension in the plot.}

\item{dx}{If \code{what = "effect"}, the variable whose conditional marginal effect should be displayed. By default it is \code{x} (so the plot displays the marginal effect of \code{x} across values of \code{x}); ignored otherwise. If \code{dx} is a factor with more than 2 levels, an error will be issued.}

\item{what}{A character string specifying whether to draw a \dQuote{prediction} (fitted values from the model, calculated using \code{\link[stats]{predict}}) or an \dQuote{effect} (average marginal effect of \code{dx} conditional on \code{x}, using \code{\link{margins}}). Methods for classes other than \dQuote{lm} or \dQuote{glm} may provided additional options (e.g., \code{cplot.polr()} provides \dQuote{stackedprediction} and \dQuote{class} alternatives).}

\item{data}{A data frame to override the default value offered in \code{object[["model"]]}.}

\item{type}{A character string specifying whether to calculate predictions on the response scale (default) or link (only relevant for non-linear models).}

\item{vcov}{A matrix containing the variance-covariance matrix for estimated model coefficients, or a function to perform the estimation with \code{model} as its only argument.}

\item{at}{Currently ignored.}

\item{n}{An integer specifying the number of points across \code{x} at which to calculate the predicted value or marginal effect, when \code{x} is numeric. Ignored otherwise.}

\item{xvals}{A numeric vector of values at which to calculate predictions or marginal effects, if \code{x} is numeric. By default, it is calculated from the data using \code{\link[prediction]{seq_range}}. If \code{x} is a factor, this is ignored, as is \code{n}.}

\item{level}{The confidence level required (used to draw uncertainty bounds).}

\item{draw}{A logical (default \code{TRUE}), specifying whether to draw the plot. If \code{FALSE}, the data used in drawing are returned as a list of data.frames. This might be useful if you want to plot using an alternative plotting package (e.g., ggplot2). Also, if set to value \dQuote{add}, then the resulting data is added to the existing plot.}

\item{xlab}{A character string specifying the value of \code{xlab} in \code{\link[graphics]{plot}}.}

\item{ylab}{A character string specifying the value of \code{ylab} in \code{\link[graphics]{plot}}.}

\item{xlim}{A two-element numeric vector specifying the x-axis limits. Set automatically if missing.}

\item{ylim}{A two-element numeric vector specifying the y-axis limits. Set automatically if missing.}

\item{lwd}{An integer specifying the width of the prediction or marginal effect line. See \code{\link[graphics]{lines}}. If \code{x} is a factor variable in the model, this is used to set the line width of the error bars.}

\item{col}{A character string specifying the color of the prediction or marginal effect line. If \code{x} is a factor variable in the model, this is used to set the color of the error bars.}

\item{lty}{An integer specifying the \dQuote{line type} of the prediction or marginal effect line. See \code{\link[graphics]{par}}. If \code{x} is a factor variable in the model, this is used to set the line type of the error bars.}

\item{se.type}{A character string specifying whether to draw the confidence interval as \dQuote{lines} (the default, using \code{\link[graphics]{lines}}) or a \dQuote{shade} (using \code{\link[graphics]{polygon}}).}

\item{se.col}{If \code{se.type = "lines"}, a character string specifying the color of the confidence interval lines. If \code{se.type = "shade"}, the color of the shaded region border.}

\item{se.fill}{If \code{se.type = "shade"}, the color of the shaded region. Ignored otherwise.}

\item{se.lwd}{If \code{se.type = "lines"}, the width of the confidence interval lines. See \code{\link[graphics]{lines}}.}

\item{se.lty}{If \code{se.type = "lines"}, an integer specifying the \dQuote{line type} of the confidence interval lines; if \code{se.type = "shade"}, the line type of the shaded polygon border. See \code{\link[graphics]{par}}.}

\item{factor.lty}{If \code{x} is a factor variable in the model, this is used to set the line type of an optional line connecting predictions across factor levels. If \code{factor.lty = 0L} (the default), no line is drawn.. See \code{\link[graphics]{par}}.}

\item{factor.pch}{If \code{x} is a factor variable in the model, the shape to use when drawing points. See \code{\link[graphics]{points}}.}

\item{factor.col}{If \code{x} is a factor variable in the model, the color to use for the border of the points. See \code{\link[graphics]{points}}.}

\item{factor.fill}{If \code{x} is a factor variable in the model, the color to use for the fill of the points. See \code{\link[graphics]{points}}.}

\item{factor.cex}{If \code{x} is a factor variable in the model, the \dQuote{expansion factor} to use for the point size. See \code{\link[graphics]{points}}.}

\item{xaxs}{A character string specifying \code{xaxs}. See \code{\link[graphics]{par}}.}

\item{yaxs}{A character string specifying \code{xaxs}. See \code{\link[graphics]{par}}.}

\item{las}{An integer string specifying \code{las}. See \code{\link[graphics]{par}}.}

\item{scatter}{A logical indicating whether to plot the observed data in \code{data} as a scatterplot.}

\item{scatter.pch}{If \code{scatter = TRUE}, an integer specifying a shape to use for plotting the data. See \code{\link[graphics]{points}}.}

\item{scatter.col}{If \code{scatter = TRUE}, a character string specifying a color to use for plotting the data. See \code{\link[graphics]{points}}.}

\item{scatter.bg}{If \code{scatter = TRUE}, a character string specifying a color to use for plotting the data. See \code{\link[graphics]{points}}.}

\item{scatter.cex}{If \code{scatter = TRUE}, an integer specifying the size of the points. See \code{\link[graphics]{points}}.}

\item{rug}{A logical specifying whether to include an x-axis \dQuote{rug} (see \code{\link[graphics]{rug}}).}

\item{rug.col}{A character string specifying \code{col} to \code{\link[graphics]{rug}}.}

\item{rug.size}{A numeric value specifying \code{ticksize} to \code{\link[graphics]{rug}}.}
}
\value{
A tidy data frame containing the data used to draw the plot. Use \code{draw = FALSE} to simply generate the data structure for use elsewhere.
}
\description{
Draw one or more conditional effects plots reflecting predictions or marginal effects from a model, conditional on a covariate. Currently methods exist for \dQuote{lm}, \dQuote{glm}, \dQuote{loess} class models.
}
\details{
Note that when \code{what = "prediction"}, the plots show predictions holding values of the data at their mean or mode, whereas when \code{what = "effect"} average marginal effects (i.e., at observed values) are shown.

When examining generalized linear models (e.g., logistic regression models), confidence intervals for predictions can fall outside of the response scale (again, for logistic regression this means confidence intervals can exceed the (0,1) bounds). This is consistent with the behavior of \code{\link[stats]{predict}} but may not be desired. The examples (below) show ways of constraining confidence intervals to these bounds.

The overall aesthetic is somewhat similar to to the output produced by the \code{marginalModelPlot()} function in the \bold{\href{https://cran.r-project.org/package=car}{car}} package.
}
\examples{
\dontrun{
require('datasets')
# prediction from several angles
m <- lm(Sepal.Length ~ Sepal.Width, data = iris)
cplot(m)

# more complex model
m <- lm(Sepal.Length ~ Sepal.Width * Petal.Width * I(Petal.Width ^ 2), 
        data = head(iris, 50))
## marginal effect of 'Petal.Width' across 'Petal.Width'
cplot(m, x = "Petal.Width", what = "effect", n = 10)

# factor independent variables
mtcars[["am"]] <- factor(mtcars[["am"]])
m <- lm(mpg ~ am * wt, data = mtcars)
## predicted values for each factor level
cplot(m, x = "am")
## marginal effect of each factor level across numeric variable
cplot(m, x = "wt", dx = "am", what = "effect")

# marginal effect of 'Petal.Width' across 'Sepal.Width'
## without drawing the plot
## this might be useful for using, e.g., ggplot2 for plotting
tmp <- cplot(m, x = "Sepal.Width", dx = "Petal.Width", 
             what = "effect", n = 10, draw = FALSE)
if (require("ggplot2")) {
  # use ggplot2 instead of base graphics
  ggplot(tmp, aes(x = Petal.Width, y = "effect")) + 
         geom_line(lwd = 2) + 
         geom_line(aes(y = effect + 1.96*se.effect)) + 
         geom_line(aes(y = effect - 1.96*se.effect))
}

# a non-linear model
m <- glm(am ~ wt*drat, data = mtcars, family = binomial)
cplot(m, x = "wt") # prediction (response scale)
cplot(m, x = "wt") # prediction (link scale)
if (require("ggplot2")) {
  # prediction (response scale, constrained to [0,1])
  cplotdat <- cplot(m, x = "wt", type = "link", draw = FALSE)
  ggplot(cplotdat, aes(x = xvals, y = plogis(yvals))) + 
         geom_line(lwd = 1.5) + 
         geom_line(aes(y = plogis(upper))) + 
         geom_line(aes(y = plotis(lower)))
}

# effects on linear predictor and outcome
cplot(m, x = "drat", dx = "wt", what = "effect", type = "link")
cplot(m, x = "drat", dx = "wt", what = "effect", type = "response")

# plot conditional predictions across a third factor
local({
  iris$long <- rbinom(nrow(iris), 1, 0.6)
  x <- glm(long ~ Sepal.Width*Species, data = iris)
  cplot(x, x = "Sepal.Width", data = iris[iris$Species == "setosa", ], 
        ylim = c(0,1), col = "red", se.fill = rgb(1,0,0,.5), xlim = c(2,4.5))
  cplot(x, x = "Sepal.Width", data = iris[iris$Species == "versicolor", ], 
        draw = "add", col = "blue", se.fill = rgb(0,1,0,.5))
  cplot(x, x = "Sepal.Width", data = iris[iris$Species == "virginica", ], 
        draw = "add", col = "green", se.fill = rgb(0,0,1,.5))
})

# ordinal outcome
if (require("MASS")) {
  # x is a factor variable
  house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, 
                    data = housing)
  ## predicted probabilities
  cplot(house.plr)
  ## cumulative predicted probabilities
  cplot(house.plr, what = "stacked")
  ## ggplot2 example
  if (require("ggplot2")) {
    ggplot(cplot(house.plr), aes(x = xvals, y = yvals, group = level)) + 
      geom_line(aes(color = level))
  }

  # x is continuous
  cyl.plr <- polr(factor(cyl) ~ wt, data = mtcars)
  cplot(cyl.plr, col = c("red", "purple", "blue"), what = "stacked")
  cplot(cyl.plr, what = "class")
}

}
}
\seealso{
\code{\link{plot.margins}}, \code{\link{persp.lm}}
}
\keyword{graphics}
