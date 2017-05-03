\documentclass{article}
\usepackage[urw-garamond]{mathdesign}
\usepackage{hyperref}
\usepackage{sidenotes}
\usepackage[font={it,small},labelfont={sc,small}]{caption}
\usepackage{dot2texi}
\usepackage{tikz}
\newcommand{\Expect}[1]{\mathbb{E}\left[ #1 \right]}
\newcommand{\Var}[1]{\mathbb{V}\left[ #1 \right]}
\newcommand{\Prob}[1]{\Pr{\left[ #1 \right]}}
\begin{document}
\title{Exam 3: Solutions}
\author{36-402, Spring 2015}
\date{Not for circulation, even after the semester is over}
\maketitle

<< echo=FALSE >>=
library(knitr)
opts_chunk$set(size='small',background='white',cache=TRUE,autodep=TRUE)
@

<<include=FALSE>>=
ckm <- read.csv("http://www.stat.cmu.edu/~cshalizi/uADA/15/exams/3/ckm-nodes.csv")
net <- as.matrix(read.table("http://www.stat.cmu.edu/~cshalizi/uADA/15/exams/3/ckm-net.dat"))
@

On loading the files, we find that the adoption date is missing for
\Sexpr{sum(is.na(ckm[,"adoption_date"]))} doctors.  Since they are useless for
our purposes, we drop them, being careful to also drop their rows and columns
from the network.\footnote{We could try to impute the adoption dates, but in
  some cases doctors missing adoption dates are also missing every other
  covariate except their location in the network\ldots}

<<include=FALSE>>=
good.drs <- which(!is.na(ckm$adoption_date))
ckm <- ckm[good.drs,]
net <- net[good.drs,good.drs]
@

<<include=FALSE>>=
# Nicely-formatted vector of dates, for later use
library(zoo) # for the "yearmon" date class
the.months <- as.yearmon(1953 + seq(from=(11-1)/12,by=1/12,length.out=17))
  # Class records January 1953 as 1953.0, Feb. 1953 as 1953+1/12, etc.
@


\marginpar{Problem 1}
% (5) Create a plot of the number of doctors who {\em began} prescribing
% tetracycline each month versus time.  (It is OK for the numbers on the
% horizontal axis to just be integers rather than formatted dates.)  Produce
% another plot of the {\em total} number of doctors prescribing tetracycline in
% each month.  (The curve for total adoptions should first rise rapidly and
% then level out around month 6.)
We begin by looking, simply, at how many doctors adopted each month (Figure
\ref{fig:adoptions}).  The number of new adoptions each month is somewhat
erratic but trends down.  The cumulative number of doctors prescribing the drug
thus increases sharply at first, but does indeed level off mid-way through
1954.

\begin{figure}
\centering
<<adoptions.per.month,echo=FALSE,out.width="0.5\\textwidth">>=
new.adoptions <- sapply(1:17, function(x) { sum(ckm$adoption_date == x) })
plot(the.months, new.adoptions, xlab="Date", ylab="New adoptions that month",
     las=2)
@
<<cumul.adoptions,echo=FALSE,out.width="0.5\\textwidth">>=
cum.adoptions <- cumsum(new.adoptions)
plot(the.months, cum.adoptions, xlab="Date",
     ylab="Total adoptions by that month",ylim=c(0,125),
     las=2)
@
\caption{Number of doctors who began prescribing tetracycline each month
  (above), and the cumulative number of doctors prescribing (below).}
\label{fig:adoptions}
\end{figure}

\marginpar{Problem 2}
% Estimate the probability that a doctor who had not yet adopted the drug will begin to do so in a given month $t$, as a function of the {\em total} number of doctors $N_t$ who had adopted {\em before} $t$.  (You may assume that these probabilities are the same for all $t$.)  You may estimate this function however you like, but be sure to explain how you are estimating these probabilities, and how you know that method is reliable {\em in this   particular case}.  (This may involve model checking.)
One possible explanation for such a pattern is that doctors pick up the use of
the new drug from each other, through example or peer pressure.  This suggests
that adoption should be more probable when more doctors have already
adopted, so we would like to estimate the probability of adoption
(by doctors who have not yet adopted) as a function of the number of
previous adopters.

There are at least two ways of going about this.  One is to directly estimate
the probability of adoptions in month $t$, as the proportion of eligible
doctors actually adopting,
\[
  p_t = \frac{\# \mathrm{new\ adoptions\ in\ month}\ t}{\# \mathrm{doctors\
      not\ adopting\ before}\ t} = \frac{\# \mathrm{new\ adoptions\ in\ month}\
    t}{125-N_t}
\]
and then regress $p_t$ on $N_t$.  To do this regression really properly, we
should weight each value of $t$, with months with more-precisely estimated
values of $p_t$ get more weight, and perhaps transform $p_t$ so that its range
is unlimited, and the estimated regression function would be guaranteed to
always give a valid probability.

The other approach is to use a logistic link function to model the number of
``successes'' (adoptions) each month; any sort of logistic model predicts
probabilities, so this can be used.  If we observe a binomial count of known
size, rather than just a binary response, we can still estimate a logistic
model using \verb+glm+ or \verb+gam+; we just have to make the left-hand side
of the formula contain two columns, one counting successes and the other
failures.  (See \verb+?glm+, first paragraph of ``Details''.)

While both approaches are valid, for brevity's sake these solutions will just
go through the second.

The two columns of the data frame which the hint directs us to build are the
two variables we just plotted, only with the cumulative number of adoptions
shifted back by one month.  These instructions leave ambiguous what we should
do for month 1: do we say $N_1 = 0$ (because nobody was {\em observed} to
prescribe before month 1), or $N_1 = $ \verb+NA+ (because we don't {\em know}
how many)?  The latter seems slightly more reasonable, but the former is also
defensible.

Accordingly, we fit a GAM where the response variable is new adoptions among
those who could adopt, the only predictor variable is $N_t$, the link function
is logistic, and we smooth $N_t$ with a spline.  This should work, because it
imposes essentially no restrictions on how $p_t$ varies with $N_t$, except that
it not change too abruptly (and even then, strong evidence will pick a low
amount of smoothing in cross-validation).

<<include=FALSE>>=
df.2 <- data.frame(new.adopt=new.adoptions,
                   prev.adopt=c(NA,head(cum.adoptions,-1)))
df.2$could.adopt <- 125-df.2$prev.adopt
library(mgcv)
gam.2 <- gam(cbind(new.adopt,could.adopt-new.adopt) ~ s(prev.adopt),
             data=df.2, family="binomial")
@


% (5) Report these probabilities as a curve, with $N$ ranging from 0 to 125.
% If you do not think you can estimate the whole range, plot as much as
% you can, and explain why you cannot go further.  For full credit, your plot
% must have more than 17 points.  Also for full credit, your curve should be
% accompanied by some measure of its error.

\marginpar{Problem 2a} Having estimate the model, we'll look at what it
predicts $p$ should be as a function of $N$, over the whole range $0$ to $125$.
There are no months where $N$ is that small or that large, but having estimated
a model, it will quite cheerfully predict $p$ at any arbitrary value of $N$, so
we can sweep out a curve like the black line in Figure \ref{fig:pred2}.  This
shows an initial increase from a low level to an adoption probability of
$\approx 0.2$ per doctor per month, then a slight decrease at large $N$.  This
is in reasonable agreement with the observed values of $p_t$, especially once
we give them error bars ($\pm 2$ standard errors).

There are several valid ways to attach a measure of uncertainty to the curve in
Figure \ref{fig:pred2}.  While resampling of residuals is hard to do with a
discrete response variable, we can generate a bootstrap replicate by simulating
from the model, which is what generates the blue 95\% bootstrap confidence
bands in the figure.  Resampling rows is also possible, though it requires a
little more care than usual, since some rows of our data frame really represent
more observations than others.  Finally, in this case the standard errors
calculated by \texttt{gam} are actually in pretty good agreement with
bootstrapping (grey curves).\marginpar{See extra credit section at end for a
  subtler, time-series bootstrap}

A calibration plot (Figure \ref{fig:calibration2}) summarizes the fit of the
observed proportions to the predicted probabilities can be summarized in a
calibration plot , which shows nothing very alarming, though the range for both
variables is fairly small and the proportions are not terribly precise.

<<include=FALSE>>=
preds.2 <- predict(gam.2, type="response", se.fit=TRUE,
                   newdata=data.frame(prev.adopt=0:125))
@

<<include=FALSE>>=
# The usual bootstrap song and dance
resample <- function(x) { sample(x,size=length(x),replace=TRUE) }
resample.data.frame <- function(df) { df[resample(1:nrow(df)),] }
boot.se <- function(B,estimator,data,simulator) {
    tboot <- matrix(replicate(B, estimator(simulator(data))),ncol=B)
    # Usually, tboot is a matrix, but if estimator returns only one number,
    # might just be a vector...
    return(apply(tboot,1,sd))
}
boot.cis <- function(B,alpha,estimator,data,simulator) {
    tboot <- matrix(replicate(B, estimator(simulator(data))),ncol=B)
    low.quantiles <- apply(tboot,1,quantile,probs=alpha/2)
    high.quantiles <- apply(tboot,1,quantile,probs=1-alpha/2)
    point.ests <- estimator(data)
    low.cis <- 2*point.ests - high.quantiles
    hi.cis <- 2*point.ests - low.quantiles
    return(cbind(low.cis,hi.cis))
}
@

<<include=FALSE>>=
# Simulate binomial counts from a model based on monthly adoption counts
# Inputs: data frame providing predictor variables, fitted model
# Outputs: New data frame with response column replaced
# Presumes: proper names in data, hard-coded variable names
sim.counts <- function(data,mdl=gam.2) {
    p <- predict(mdl,newdata=data,type="response")
    data$new.adopt <- rbinom(length(p),size=data$could.adopt,prob=p)
    return(data)
}
@

<<include=FALSE>>=
# Fit the binomial-counts GAM to a data set, and then evaluate its predictions
# along the specified range of counts
# ATTN: Lots of stuff hard-coded in this which could be made more flexible
counts.curve <- function(df) {
    mdl <- gam(formula=formula(gam.2),data=df,family="binomial")
    preds <- predict(mdl,type="response",newdata=data.frame(prev.adopt=0:125))
    return(preds)
}

pred2.cis <- boot.cis(1000,0.05,estimator=counts.curve,simulator=sim.counts,
                      data=df.2)
@

\begin{figure}
  \centering
<<echo=FALSE, out.width="0.5\\textwidth">>=
plot(0:125,preds.2$fit,type="l",ylim=c(0,1),xlab=expression(N[t]),
     ylab=expression(p[t]))
# +- 2 nominal standard errors
lines(0:125,preds.2$fit-2*preds.2$se.fit,col="grey")
lines(0:125,preds.2$fit+2*preds.2$se.fit,col="grey")
# 95% confidence band by bootstrap
lines(0:125,pred2.cis[,1],col="blue")
lines(0:125,pred2.cis[,2],col="blue")
obs.prop <- with(df.2, new.adopt/could.adopt)
with(df.2,points(prev.adopt,obs.prop,col="red"))
prop.pm.se <- function(n,x,pm) {
    p <- x/n
    se <- sqrt(p*(1-p)/n)
    if (pm=="plus") { return (p+2*se) }
    else { return(p-2*se) }
}
with(df.2,segments(prev.adopt,prop.pm.se(could.adopt,new.adopt,"plus"),
                   prev.adopt,prop.pm.se(could.adopt,new.adopt,"minus"),
                   col="red"))
@
\caption{Adoption probability as a function of $N$, $N \in 0:125$.  The black
  curve is the prediction of our estimated logistic GAM; the red circles are
  the observed proportions, $\pm 2$ standard errors (assuming each doctor makes
  an independent decision each month).  Grey curves, $\pm 2$ standard errors
for the predictions, as calculated by \texttt{gam}; blue curves, 95\%
confidence bands, as found by a model-based bootstrap with 1000 replicates.}
\label{fig:pred2}
\end{figure}

\begin{figure}[t]
\centering
<<echo=FALSE, out.width="0.45\\textwidth">>=
obs.prop <- na.omit(obs.prop)
plot(fitted(gam.2),obs.prop,xlim=c(0,0.4),ylim=c(0,0.4),
     xlab="Predicted probability",ylab="Observed proportion")
with(df.2[-1,],segments(fitted(gam.2),prop.pm.se(could.adopt,new.adopt,"plus"),
                   fitted(gam.2),prop.pm.se(could.adopt,new.adopt,"minus")))
abline(0,1,lty="dotted")
@
\caption{Calibration plot for the logistic GAM predicting adoption probability
  as a function of $N$.  Since there are multiple doctors with the same
  predicted probability of adoption each month, dots and their confidence bands
  here show how well the observed proportion for each month agrees with that
  month's predicted probability.}
\label{fig:calibration2}
\end{figure}


% 15
% (5) Averaging over doctors and months, how much does the predicted
% probability of adoption change when $N$ increases by $1$?  Give a standard
% error to this change in predicted
% probabilities.
\marginpar{Problem 2b}
\textsc{Solution:} Our model gives us, for each month, a predicted probability
that every doctor who could adopt that month will do so.  This probability is
solely a function of $N$, $\hat{p}(N)$, and the model will also make a predict
of what would have happened in month $t$ if $N=N_t+1$ (as opposed to its actual
value of $N_t$).  Thus we want to average $\hat{p}(N_t+1)-\hat{p}(N_t)$,
weighting each month $t$ by the number of doctors who could have adopted this
month.  This is exactly the same principle as the ``average predictive
comparisons'' of \S 4.5 of the notes (and the accompanying homework), though
with a much simpler calculation, since we're only changing one variable and we
are always changing it by the same amount.  If our model were linear, this
would just be the coefficient on $N$; fitting a linear model to our
predictions, etc., is not an acceptable substitute for actually doing the
calculation.

<<include=FALSE>>=
effect.of.increasing.N.by.one <- function(data,form=formula(gam.2)) {
    mdl <- gam(formula=form,data=data,family="binomial")
    baseline <- predict(mdl,newdata=data,type="response")
    plus.one <- data
    plus.one$prev.adopt <- plus.one$prev.adopt+1
    newpreds <- predict(mdl,newdata=plus.one,type="response")
    return(weighted.mean(newpreds-baseline,w=data$could.adopt,na.rm=TRUE))
}

point.estimate.of.increment <- effect.of.increasing.N.by.one(df.2)
se.increment <- boot.se(1000,estimator=effect.of.increasing.N.by.one,
                        simulator=sim.counts,
                        data=df.2)
@

Actually doing the calculation gives an average change in probability of
$\Sexpr{signif(point.estimate.of.increment,3)}$, with a standard error, from
1000 model-based bootstraps, of $\pm\Sexpr{signif(se.increment,3)}$.



%\clearpage

% Estimate the probability that a doctor $i$ who had not yet adopted the
% drug will begin to do so in month $t$, as a function of the number $C_{it}$
% of {\em doctors linked to $i$} who had adopted before $t$.  (Again, you may
% assume that these probabilities are the same for all $t$.)
\marginpar{Problem 3} The flattering of the adoption-probability curve in
Figure \ref{fig:pred2} suggests that past a certain point, having more doctors
adopt has little effect on the probability of further adoptions.  However, this
involves looking at the total number of adopters, which, sociologically, is a
bit odd --- why should a doctor in Peoria care about the prescriptions of some
other doctor he doesn't know in Quincy?  A more reasonable model would predict
adoption for each doctor from the number of his\footnote{The gendered language
  seems historically appropriate.} friends and contacts who have adopted.

To estimate such a model, we build a new data frame with a row for every doctor
and every month, as suggested in the hint.  The construction can be done with a
\verb+for+ loop, or, somewhat more elegantly, by using \verb+expand.grid+ to
come up with all combinations of doctors and months, and then using
\verb+apply+.  (See code online.)  The result has $\Sexpr{nrow(ckm)} \times 17
= \Sexpr{nrow(ckm)*17}$ rows, and five columns, recording doctor ($i$), month
($t$), whether that doctor adopted that month, whether he had previously
adopted, and the number of his contacts who had previously adopted ($C_{it}$).

<<include=FALSE>>=
df.3 <- expand.grid(doctor=1:nrow(ckm),month=1:17)
# Which doctors does a given doctor know?
contacts <- function(dr) { which(net[dr,]==1) }
# When do a given doctor's contacts adopt?
contact.dates <- function(dr) { ckm[contacts(dr),"adoption_date"] }
# How many of a given doctor's contacts adopt before a given month?
prev.peers <- function(doctor,month) { sum(contact.dates(doctor) < month) }
library(plyr)
peer.pressure <- maply(df.3,prev.peers,.expand=FALSE)
  # NOTE: Instead of using the plyr library's maply function, I could have
  # written prev.peers() to take a single vector argument, as it were c(dr,mo),
  # and then used apply(df.3,1,prev.peers); this seemed a little nicer.
adopting <- maply(df.3,
                  function(doctor,month) { ckm[doctor,"adoption_date"]==month },
                  .expand=FALSE)
already <- maply(df.3,
                  function(doctor,month) { ckm[doctor,"adoption_date"]<month },
                  .expand=FALSE)
df.3 <- cbind(df.3,adopting,already,peer.pressure)
@

We again fit a logistic GAM to this data, with the binary response being
whether the doctor adopted that month, and the sole predictor variable being
$C_{it}$.  Again, because we're conditioning on only one numerical variable,
such a GAM will, with enough data, recover the true conditional probability
function, whatever it might be --- though we might need a lot of data if the
true function is very erratic.  When doing so, we only include rows where the
doctor has {\em not} previously adopted, since we're told to ``estimate the
probability that a doctor $i$ who had not yet adopted the drug will begin to do
so in month $t$''.

<<include=FALSE>>=
gam.3 <- gam(adopting ~ s(peer.pressure), data=df.3,
             subset=which(df.3$already==0), family="binomial")
@


\marginpar{Problem 3a}
% (8) Make a plot of these probabilities, with $C_{it}$ ranging from 0 to
% 30.  If you do not think you can estimate the whole range, plot as much as
% you can, and explain why you cannot go further.  For full credit, your plot
% must include at least 29 points, and include a measure of uncertainty in
% your estimates.  Does your curve support the idea that the use of
% tetracycline is transmitted from one doctor to another through the social
% network?  Explain, including a description of what curves which did {\em
%   not} support this idea would look like, or why the shape of this curve is
% actually irrelevant to this issue.
Figure \ref{fig:pred3} plots the model's predictions for the probability of
adoption as a function of $C$, with $C$ ranging from 0 to 30.  Since the
maximum observed value of $C$ is \Sexpr{max(df.3[,"peer.pressure"])}, going
over this full range is only possible with a model, rather than trying to
calculate observed proportions for each value of $C$.  (Indeed, the maximum
number of {\em contacts} any doctor has is \Sexpr{max(rowSums(net))}.)  The
figure again shows both \verb+gam()+'s standard errors, and the 95\% confidence
bands from 1000 simulations (model-based bootstraps) of the model.

The curve rises monotonically (and nonlinearly) with $C$, which is exactly what
we would expect if doctors adopt the new drug through a process of social
influence or (like other drugs) peer pressure.  A curve which was flat or
declining in $C$, or even one which first rose and then declined in $C$, would
be much harder to explain as a consequence of social influence.  However, once
$C$ gets above 5 or so, the confidence bands become {\em extremely} wide ---
the model is happy to make predictions about what will happen out there, but
those predictions are just wild guesses, extrapolating from the few data points
off in the right tail, and the uncertainty reflects this.

\begin{figure}
  \centering
<<include=FALSE, out.width="0.5\\textwidth">>=
preds.3 <- predict(gam.3,newdata=data.frame(peer.pressure=0:30),
                   type="response",se.fit=TRUE)
@
<<include=FALSE>>=
# Simulate binary response from a fitted model
# Inputs: data frame providing predictor variables, fitted model
# Outputs: New data frame with response column replaced
# Presumes: proper names in data, hard-coded variable names
sim.binary <- function(data,mdl=gam.3) {
    p <- predict(mdl,newdata=data,type="response")
    data$adopting <- rbinom(length(p),size=1,prob=p)
    return(data)
}

# Fit the binary-response logistic GAM to data, then evaluate its predictions
  # along the specified range of number of previous adopters
# ATTN: Strongly parallel to counts.curve, but both have lots of details
  # hard-coded; should really write a general version
binary.curve <- function(df) {
    # Note; subset argument to gam does not play well with being called inside
    # another function
    mdl <- gam(formula=formula(gam.3),data=df[df[,"already"]==0,],
               family="binomial")
    preds <- predict(mdl,type="response",newdata=data.frame(peer.pressure=0:30))
    return(preds)
}


pred3.cis <- boot.cis(1000,0.05,estimator=binary.curve,simulator=sim.binary,
                      data=df.3)

@

\centering

<<echo=FALSE, out.width="0.5\\textwidth">>=
plot(0:30, preds.3$fit, xlab="Number of contacts previously adopting",
     ylab="Adoption probability", ylim=c(0,1),type="l")
rug(x=jitter(df.3$peer.pressure[df.3$already==0]),side=1)
lines(0:30, pmin(1,preds.3$fit+2*preds.3$se.fit),col="grey")
lines(0:30, pmax(0,preds.3$fit-2*preds.3$se.fit),col="grey")
lines(0:30, pmax(0,pred3.cis[,1]),col="blue")
lines(0:30, pmin(1,pred3.cis[,2]),col="blue")
@
\caption{Adoption probability (per doctor per month) as a function of the
  number of the doctor's contacts who had previously adopted, $C \in 0:30$.
  Black curve, prediction of the logistic GAM.  Grey curves, $\pm 2$ nominal
  standard errors from \texttt{gam()}. Blue curves, 95\% confidence intervals
  from 1000 model-based bootstraps.  Limits are capped at 0 and 1.  Tick-marks
  on the horizontal axis show the observed values of $C$, jittered to bring out
  their relative frequency.}
\label{fig:pred3}
\end{figure}

\begin{figure}
\centering
<<echo=FALSE,out.width="0.45\\textwidth">>=
# Calibration-plot code modified from solutions to homework 6

frequency.vs.probability <- function(p.lower,p.increment=0.01,
  model,events) {
  fitted.probs <- fitted(model)
  indices <- (fitted.probs >= p.lower) & (fitted.probs < p.lower+p.increment)
  ave.prob <- mean(fitted.probs[indices])
  frequency <- mean(events[indices])
  individual.vars <- fitted.probs[indices]*(1-fitted.probs[indices])
  var.of.average <- sum(individual.vars)/(sum(indices)^2)
  se <- sqrt(var.of.average)
  out <- c(frequency=frequency,ave.prob=ave.prob,se=se)
  return(out)
}
f.vs.p.3 <- data.frame(t(sapply((0:8)/20,frequency.vs.probability,
                              p.increment=0.1,
                              model=gam.3,
                              events=df.3$adopting[df.3$already==0])))
plot(frequency~ave.prob, data=f.vs.p.3, xlab="Predicted probabilities",
     ylab="Observed frequencies", xlim=c(0,1), ylim=c(0,1))
rug(fitted(gam.3),col="lightgrey")
abline(0,1,col="darkgrey")
segments(x0=f.vs.p.3$ave.prob,y0=f.vs.p.3$ave.prob-1.96*f.vs.p.3$se,
  y1=f.vs.p.3$ave.prob+1.96*f.vs.p.3$se)
@
\caption{Calibration plot for the logistic GAM based on the number of contacts.}
\label{fig:calibration3}
\end{figure}

\marginpar{Problem 3b}
% (7) Averaging over doctors and months, how much does the predicted
% probability of adoption change when $C_{it}$ increases by one?  What is
% your standard error for this change in predicted
% probabilities?
Since the new model gives us a prediction for the probability of adoption
solely as a function of the number of previously-adopting contacts $C$,
$\hat{p}(C)$.  We are interested in how this changes if $C$ increases by 1,
i.e., in $\hat{p}(C_{it}+1) - \hat{p}(C_{it})$.  We average over all values of
$i$ and $t$ where the doctor could in fact have adopted.

<<include=FALSE>>=
effect.of.increasing.C.by.one <- function(data,form=formula(gam.3)) {
    data <- data[data$already==0,]
    mdl <- gam(formula=form,data=data,family="binomial")
    baseline <- predict(mdl,newdata=data,type="response")
    plus.one <- data
    plus.one$peer.pressure <- plus.one$peer.pressure+1
    newpreds <- predict(mdl,newdata=plus.one,type="response")
    return(mean(newpreds-baseline,na.rm=TRUE))
}

point.estimate.of.increment <- effect.of.increasing.C.by.one(df.3)
se.increment <- boot.se(1000,estimator=effect.of.increasing.C.by.one,
                        simulator=sim.binary,
                        data=df.3)

@

Doing so, we find that the probability changes, on average, by
\Sexpr{signif(point.estimate.of.increment,3)}, with a standard error of $\pm
\Sexpr{signif(se.increment,3)}$ (again, from 1000 simulations of the model).


\marginpar{Problem 4a}
% (1) Are your estimates from problem \ref{problem:one-doctor-total} and
% \ref{problem:one-known-doctor} consistent with one another?  Explain.
We now have two claims about how our predicted probability of any given
doctor's adopting the drug should typically change when we learn that one more
doctor has already adopted.  In a {\em very} qualitative way, these claims are
compatible with each other, since they both indicate that adoption becomes more
likely in the future if more doctors have adopted already.  Since, however, the
two estimates come from models with quite distinct predictor variables, there's
no intrinsic reason they even have to have the same sign!  (The predictor
variables aren't totally distinct, $N_t = \sum_{i}{C_{it}}$, but that does
little to relate the two models.)

\marginpar{Problem 4b}
% (4) What would you have to assume for either of these to be estimates of
% the causal effect on adoption by other doctors of making one extra doctor
% adopt the drug?  Be as specific as you can, rather than just repeating
% definitions from the notes.  Drawing graphs is encouraged.
What we'd need to assume for either of these numbers to be good estimates of
the causal effect of making one more doctor adopt\footnote{Something we might
  want to know if, e.g., we're the marketing department for the maker of
  tetracycline.} is a trickier question.  Let's say that the variable $Y_{it}$
indicates whether doctor $i$ starts prescribing tetracycline in month $t$; thus
we've been estimating $\Prob{Y_{it}=1 \mid N_t}$ or $\Prob{Y_{it}=1 \mid
  C_{it}}$.  We're making an un-adjusted single regression estimate, so our
only hope is the back-door criterion.  To use this, there need to not be any
back-door paths from the predictor variable ($N_t$ in problem 2, $C_{it}$ in
problem 3) to the response. That is, there must not be any common causes of the
predictor and the response which we have not controlled for or conditioned on.
Because we are only looking at doctors who have not yet adopted, we are
implicitly conditioning on $Y_{i t-1}$, specifically conditioning on $Y_{i
  t-1}=0$.  Thus, we're assuming that any common cause of $N_t$ or $C_{it}$ and
$Y_{it}$ can only reach $Y_{it}$ through $Y_{i t-1}$.  (See Figure
\ref{fig:dag-for-yit-vs-cit}.)  In problem 3, we would need, in particular, to
assume that there is no variable which makes doctor $i$ persistently (that is,
for all $t$) more (or less) likely to adopt the drug, and also more (or less)
likely to associate with those who are also apt to start prescribing.  The
parallel statement is actually somewhat easier to believe for problem 2,
because we're conditioning on the same variable ($N_t$) for all doctors, but
we'd still need to assume there wasn't anything like advertising or news which
affected all doctors, some perhaps faster than others.

\begin{figure}
\centering
\includegraphics[width=0.5\textwidth]{dag-for-yit-vs-cit}
\caption{Causal graph for predicting the effect of doctor $i$'s
  previously-adopting contacts ($C_{it}$) on current adoption ($Y_{it}$).  We
  can (and should) allow previous values of $Y_{it}$ to affect previous values
  of $C_{it}$, since whether $i$ has adopted should presumably influence
  whether $i$'s neighbors adopt.  The presence of an unobserved common ancestor
  like $U$ does not lead to confounding, since we are (implicitly) conditioning
  on previous values of $Y_{it}$ by only looking at doctors who have not yet
  adopted.  The presence of an unobserved common cause like $W$ (acting through the dotted arrows) would, however,
  lead to confounding.}
\label{fig:dag-for-yit-vs-cit}
\end{figure}

\marginpar{problem 5}
% Estimate a model which predicts the probability that a doctor $i$ who had
  % not yet adopted the drug by month $t$ will begin to do so in month $t$, as a
  % function of $C_{it}$ and of the covariates which indicate when $i$ went to
  % medical school, whether they attended medical-society meetings (and if so
  % what kind), and how many medical journals they
  % read.
If doctors with certain attributes are more likely to adopt new drugs {\em
  because} of those attributes, and those traits also make them associate with
similar doctors, that would be an example of an un-controlled common cause.  We
therefore expand our model to include attributes of individual doctors, as well
as $C_{it}$.  Specifically, we use when they went to medical
school\footnote{Which might affect their willingness to try new drugs.},
whether they go to medical conferences\footnote{A source of information and of
  socialization}, and how many medical journals they read\footnote{Which might
  expose them to ads or even reliable information about new drugs.}.  This just
requires adding columns to the data frame used for the previous model, where
each column repeats the relevant information about each doctor for each month.
We again fit a logistic GAM; the additive assumption of the GAM now comes in to
play, i.e., we're hoping that there are no interactions between these
variables.


<<include=FALSE>>=
df.5 <- data.frame(df.3, medical_school=rep(ckm$medical_school,times=17),
                   attend_meetings=rep(ckm$attend_meetings, times=17),
                   medical_journals=rep(ckm$medical_journals, times=17))
gam.5 <- gam(adopting ~ s(peer.pressure) +
                 factor(medical_school) + factor(attend_meetings) +
                     factor(medical_journals), data=df.5,
             subset=which(df.5$already==0), family="binomial")
@

\marginpar{Problem 5a}
% (5) Plot the estimated probability of adoption as a function of
% $C_{it}$ for doctors who read the minimal number of journals, do not attend
% conferences, and graduated from medical school (i) in 1919 or earlier, (ii)
% in the 1920s, and (iii) in 1945 or after.  For full credit, have all three
% lines on the same plot (clearly visually distinct from each other), and
% some measure of uncertainty for each line.
It no longer makes a lot of sense to plot predicted adoption probability
against just $C$, because the other covariates will modify the prediction.  To
get a sense of how one of them, date of graduation from medical school, alters
the prediction, we plot $p$ against $C$ in the range\footnote{The problem
  statement doesn't specify the range for $C$, and we already know that all
  margins of uncertainty are going to be huge for large $C$.} 0 to 10 for three
levels of the medical school variable, setting the number of journals read to
the smallest observed value (2) and imposing non-attendance at meetings.  Since
the default standard errors of \verb+gam()+ have matched the bootstrap
confidence intervals pretty well so far, we will, for simplicity, just use them
here, though we {\em could} still bootstrap.  (Figure \ref{fig:preds5}.)

\begin{figure}
<<include=FALSE>>=
# Set up 3 data frames for our 3 cases; mostly the same so copy and then
# alter
df.5.i <- data.frame(medical_school="1919-",
                     attend_meetings="none",
                     medical_journals=min(df.5$medical_journals),
                     peer.pressure=0:10)
df.5.ii <- df.5.i; df.5.ii$medical_school="1920--1929"
df.5.iii <- df.5.i; df.5.iii$medical_school="1945+"

# Calculate predicted probabilities, with standard errors
preds.5.i <- predict(gam.5,newdata=df.5.i,type="response",se.fit=TRUE)
preds.5.ii <- predict(gam.5,newdata=df.5.ii,type="response",se.fit=TRUE)
preds.5.iii <- predict(gam.5,newdata=df.5.iii,type="response",se.fit=TRUE)
@

<<echo=FALSE>>=
# Set up the plotting window
plot(0,xlim=c(0,10),ylim=c(0,1),
     xlab="Number of contacts previously adopting",
     ylab="Adoption probability", type="l")

# Function to plot a main prediction line +- 2 standard errors
  # Dashed lines for the error bars
# Inputs: prediction object, x values, color
# Output: NULL, invisibly
plot.preds.with.ses <- function(preds,x,col) {
    lines(x,preds$fit,col=col)
    lines(x,preds$fit+2*preds$se.fit,col=col,lty="dashed")
    lines(x,preds$fit-2*preds$se.fit,col=col,lty="dashed")
    invisible(NULL)
}
plot.preds.with.ses(preds.5.i,df.5.i$peer.pressure,"black")
plot.preds.with.ses(preds.5.ii,df.5.ii$peer.pressure,"blue")
plot.preds.with.ses(preds.5.iii,df.5.iii$peer.pressure,"red")
legend("topleft",legend=c("1919-","1920s","1945+"),
       col=c("black","blue","red"),lty="solid")
@
\caption{Predicted probabilities of adoption as a function of contacts, for
  doctors who read the minimal number of journals, do not attend contacts, and
  graduated from medical school in 1919 or before, in the 1920s, and 1945 or
  later.}
\label{fig:preds5}
\end{figure}

\begin{figure}
\centering
<<echo=FALSE, out.width="0.5\\textwidth">>=
f.vs.p.5 <- data.frame(t(sapply((0:7)/20,frequency.vs.probability,
                              p.increment=0.1,
                              model=gam.5,
                              events=df.5$adopting[df.5$already==0])))
plot(frequency~ave.prob, data=f.vs.p.5, xlab="Predicted probabilities",
     ylab="Observed frequencies", xlim=c(0,1), ylim=c(0,1))
rug(fitted(gam.5),col="lightgrey")
abline(0,1,col="darkgrey")
segments(x0=f.vs.p.5$ave.prob,y0=f.vs.p.5$ave.prob-1.96*f.vs.p.5$se,
  y1=f.vs.p.5$ave.prob+1.96*f.vs.p.5$se)
@
\caption{Calibration plot for the logistic GAM with covariates from problem 5.}
\end{figure}

\marginpar{Problem 5b}
% (5) Averaging over doctors and months, how much does increasing
% $C_{it}$ by one change the probability of doctor $i$ adopting tetracycline
% in month $t$?  Include a standard error for this change in predicted
% probabilities.
In this expanded model, our predicted probability is now a function not just of
$C_{it}$ but also of the other covariates, say $S_i, A_i, J_i$ for medical
school, attending meetings, and reading medical journals.  We want to know
$\hat{p}(C_{it}+1, S_i, A_i, J_i) - \hat{p}(C_{it}, S_i, A_i, J_i)$, and to
average it over the data, i.e., all values of $i$ and $t$ where the doctor
could have adopted.

<<include=FALSE>>=
increase.C.with.covar <- function(data) {
    data <- data[data$already==0,]
    mdl <- gam(formula=formula(gam.5),data=data,family="binomial")
    baseline <- predict(mdl,newdata=data,type="response")
    plus.one <- data
    plus.one$peer.pressure <- plus.one$peer.pressure+1
    newpreds <- predict(mdl,newdata=plus.one,type="response")
    return(mean(newpreds-baseline,na.rm=TRUE))
}

point.est <- increase.C.with.covar(df.5)
se.increment <- boot.se(1000,estimator=increase.C.with.covar,
                        simulator=function(data) {sim.binary(data,gam.5)},
                        data=df.5)
@

The average change is $\Sexpr{signif(point.est,3)}$, with a standard error of
$\pm\Sexpr{signif(se.increment,3)}$.  This standard error is derived from,
again, 1000 simulations of the model, followed by re-fitting the model.

\marginpar{Problem 5c}
% (5) Under what assumptions does this give a valid estimate of the
% average causal effect of increasing $C_{it}$ by one?
For this average change to be a valid measure of the causal effect of
increasing $C$ by one, we would need to assume that the additional covariates
block off all back-door paths, without opening any new ones.  Opening a new
back-door path would involve one of the covariates being a collider, which is
very implausible since they all come prior in time to $C_{it}$.  Blocking of
back-door paths would involve, for instance, any path from $W$ in
\ref{fig:dag-for-yit-vs-cit} to any of the $Y_{i}$ variables having to pass
through these covariates.  To be substantive, if these covariates are all the
common causes of both $i$'s adoption and how many of $i$'s contacts have
adopted, we're good.  (Attributes of $i$ could be causes of $C_{it}$ by
affecting who $i$ knows, and whether they are intrinsically likely to adopt.)

\clearpage

\subsection*{Extra Credit}

As an example of an extra credit contribution, we'll write a function which
simulates a complete time-path of the adoption process, assuming it follows
either the model from problem 3 or the model from problem 5, and returns what
the data frame would have been according to this.  It takes the observed
adoptions and covariates for month 1 as given, and then randomly decides,
according to the model, who adopts each month, making these choices
independently for each doctor, with the specified probability\footnote{One
  might worry that deciding doctor $j$ adopts will cause the adoption
  probability for his contact doctor $i$ to also change, but, by the model
  specification, this only matters to $i$ next month.}.  Conceptually, this is
simple; it does however require some amount of book-keeping to keep track of
the changing values of $C_{it}$.  The simulation function returns a data frame
which looks like the one built for problem 5; to simplify comparison to
reality, there's also a function which collapses it back into a data frame with
one row per doctor, giving the adoption month.

One wrinkle to this simulation is that we have one doctor where we know their
adoption date, but are missing two of the three covariates used in the model.
We could try imputing their values, but the code simply fixes its adoption date
at its observed value.

<<include=FALSE>>=
# Simulate the time evolution of the whole network, according to the logistic
  # GAM model on previously-adopted contacts (w/ or w/o covariates)
# Inputs: previously-fit prediction model, data frame containing covariates
  # and actual adoption dates, array containing the social network,
  # number of months to run
# Output: data frame recording, for each doctor and month, whether the
  # doctor adopted that month, whether the doctor previously adopted, number of
  # doctors contacts who had previously adopted, covariates
sim.network <- function(mdl,data,net,n.months=16) {
    # Check that the node data file and the net are compatible
    stopifnot(nrow(data)==nrow(net))
    # Prepare a data frame to store the output of the simulation
    sim.df <- create.sim.df(data,net,n.months)
    # To step through the simulation, we need to remember what happened
      # last month
    old.month <- sim.df[sim.df$month==1,]
    # Now loop over the months
    for (t in 2:(n.months+1)) {
        # Generate the new month from the old
        new.month <- sim.step(old.month,mdl,data,t)
        # Now update the big dataframe storing everything
        sim.df[sim.df$month==t,] <- new.month
        # And the new month becomes the old month for the next round
        old.month <- new.month
    }
    return(sim.df)
}

# Create a data frame for the simulation
  # Utility function for sim.network()
create.sim.df <- function(data,net,n.months) {
    # Begin with all combinations of doctors and months
    df <- expand.grid(doctor=1:nrow(data), month=1:(n.months+1))
    # Add all-zero columns for recording current adoption, previous adoption,
      # and the number of contacts who have previously adopted
    df$adopting <- 0
    df$already <- 0
    df$peer.pressure <- 0
    # Copy all the covariates from the data into the new data frame
      # Data has one entry per covariate per doctor; replicate them to come
      # out to the number of months
    for (var in colnames(data)) {
        if (var != "adoption_date") {
            df[,var] <- rep(data[,var],times=(n.months+1))
        }
    }
    # The people recorded as adopting in month 1 count as new adopters
      # (though conceivably they might not be...)
    first.adopters <- which(data$adoption_date==1)
    df[(df$doctor %in% first.adopters) & (df$month==1),"adopting"] <- 1
    return(df)
}

# Do one step of the simulation, generating the new month from the old
  # Utility function for sim.network()
# Inputs: data frame for last month, fitted model, data on original run,
  # current month (last two for handling some missing values)
# Output: data frame for the new month
sim.step <- function(old.df,mdl,data,t) {
    # Copy the old month into the new month
      # Keep static covariates unchanged, also make sure that if "already" has
      # been set before it will keep being set
    new.df <- old.df
    # Anyone who adopted last month has already adopted this month
      # (if they didn't adopt last month, though, leave their "already"
      # status alone)
    new.df[,"already"] <-
        ifelse(old.df[,"adopting"]==1, 1, new.df[,"already"])
    # Count up how many contacts of each node had adopted last month
    new.df[,"peer.pressure"] <-
        sapply(new.df$doctor, contacts.already, data=new.df, net=net)
    # Generate the predictions, using just the data for this month
      # Note: at this stage all the variables in new.df are up-to-date
      # except for "adopting' (the response variable...)
    predicted.probs <- predict(mdl, newdata=new.df, type="response")
    # We can't adopt again
    predicted.probs[new.df$already==1] <- 0
    # Toss independent coins with the appropriate probabilities
    new.df[,"adopting"] <- rbinom(n=length(predicted.probs), size=1,
                                  prob=predicted.probs)
    # If we're getting an NA for a predicted probability, we'll
      # use the observed adoption ate of those nodes
    bad.nodes <- which(is.na(predicted.probs))
    new.df[bad.nodes,"adopting"] <-
        ifelse(data[bad.nodes,"adoption_date"]==t,1,0)
    return(new.df)
}

# Count how many contacts of a given doctor have already adopted before a
  # given month
  # Utility function for sim.network()
# Input: index of doctor,, data-frame in the style of problem
  # 3 or 5, social network
# Output: Number of contacts who'd previously adopted
contacts.already <- function(doctor,data,net) {
    contacts <- which(net[doctor,]==1)
    return(sum(data[contacts,"already"]==1))
}
@



<<include=FALSE>>=
# Given a data frame in the format of problem 3 or problem 5, boil back
# down to a data frame in the format of ckm
# Input: data frame
# Output: the reduced data frame, with one row per doctor
# Presumes: a column of the input is named "doctor"
collapse.time <- function(df) {
    require(plyr)
    return(ddply(.data=df,.variable="doctor",.fun=summarize.doctor))
}

# Boil down a small data frame about one doctor into constant covariates
  # and the adoption date
# Input: a small data frame
# Output: a one-row data frame with covariate information and adoption
  # date, but with per-month information removed, along with month and doctor
  # indieces
# Presumes: All rows in input refer to the same doctor
summarize.doctor <- function(df) {
    # Take covariate information (presumed constant) from the first row
    # of the data frame
    dr <- df[1,]
    # Adoption date
    dr$adoption_date <- min(which(df$adopting==1))
    dr$adopting <- NULL
    dr$already <- NULL
    dr$peer.pressure <- NULL
    dr$doctor <- NULL
    dr$month <- NULL
    return(dr)
}
@

<<include=FALSE>>=
library(igraph)

# Plot a network with nodes colored by adoption date
# Inputs: data frame containing adoption dates, adjacency matrix,
  # optional layout ((x,y) coordinates for nodes) for graph
# Output: the graph layout, invisibly
net.by.date <- function(data,net,layout=NULL,...) {
    # Load a graph-manipulating package
    require(igraph)
    # Build an (undirected) graph from the adjacency matrix
    g <- graph.adjacency(net,mode="undirected")
    # Calculate a layout for the graph
    if (is.null(layout)) {
        layout <- layout.fruchterman.reingold(g)
    }
    # Take the adoption dates from the data
    dates <- data$adoption_date
    # Replace dates of "Inf" by a number 1 bigger than the largest month
    n.dates <- length(unique(dates))
    dates <- ifelse(dates==Inf,n.dates,dates)
    # Color palette: Rainbow for finite dates, black for Inf
    colors <- c(rainbow(n.dates-1),grey(0))
    # Set the adoption month attribute for each node
    V(g)$color <- colors[dates]
    plot(g,vertex.label=NA,vertex.size=5,layout=layout,...)
    invisible(layout)
}
@

<<include=FALSE>>=
# Run the simulation 30 times from the starting configuration
sim.runs <- replicate(30,sim.network(gam.5,data=ckm,net=net,n.months=16),
                      simplify=FALSE)
# Turn each run of the simulation into a data frame of the same format as
# the original data
sim.data <- llply(sim.runs,collapse.time)
@

\begin{figure}
<<real_and_sim_nets,echo=FALSE>>=
par(mfrow=c(2,2))
# Plot the original data (and save its graph layout)
ckm.layout <- net.by.date(ckm,net,main="Data")
net.by.date(sim.data[[1]],net,layout=ckm.layout,main="Simulation 1")
net.by.date(sim.data[[2]],net,layout=ckm.layout,main="Simulation 2")
net.by.date(sim.data[[3]],net,layout=ckm.layout,main="Simulation 3")
par(mfrow=c(1,1))
@
\caption{The adoption of tetracycline in relation to the social network.  Nodes
  represent doctors, colored by the month they adopted, progressing through the
  rainbow from red for month 1 through violet for month 17, with never-adopted
  colored black.  Top left, data; other panels, three independent runs of the
  GAM model from problem 5.}
\label{fig:simulate-the-gam}
\end{figure}

As chapter 5 says, with a good simulation model, we should be able to repeat
our exploratory data analysis on it, and while the simulations shouldn't
reproduce the exploratory findings exactly, the latter should be typical
outcomes of the simulation.  To illustrate, Figure \ref{fig:simulate-the-gam}
displays the history of adoptions over the network in the data and in three
independent runs of the simulation, and Figure
\ref{fig:adoption-curves-real-vs-sim} plots the cumulative number of adoptions
in the data (as in Figure \ref{fig:adoptions}) and in the simulations; and
Figure \ref{fig:associated-adoptions} the distribution of relative timing of
adoptions across social-network neighbors.  None of these were used directly in
fitting the model, so the latter's ability to reproduce them, at least
qualitatively, is some evidence that it's accurately describing the process.

Relatedly, Figure \ref{fig:sim.bootstrap} shows the results of fitting the
model of problem 3 (using $C_{it}$ alone) to these simulations of the model
from problem 5 (using $C$ plus covariates).  A sound rule of method is that one
theory is better than another if the former can explain and predict everything
the latter can, and more besides; if the former ``encompasses'' the latter.
Some researchers, especially in econometrics, have turned this into an
``encompassing principle'' of model-checking, that a good model should be able
to predict the parameter estimates we get from a worse model, but not vice
versa.  Here we see that the expanded model is indeed encompassing the smaller
one, at least where there's any data to support the small model.


In addition to helping to see, visually, what the model may or may not be
getting right, the simulation could be used as a model-based bootstrap.  One
advantage of this is that it would guarantee internal consistency between the
each doctor's adoption decisions and the cumulative number of neighbors who
have adopted.  Modifying the existing bootstrap code to use this simulator
rather than another is not hard, but is left as a last exercise.

\begin{figure}
<<echo=FALSE>>=
# Slightly more generic function to count cumulative adoptions (only)
adoption.curve <- function(data,n.months=17) {
    sapply(1:n.months, function(t) { sum(data$adoption_date <= t) })
}
# Create an empty plot window with nice axes etc.
plot(the.months,adoption.curve(ckm),xlab="Date",
     ylab="Total adoptions by that month",ylim=c(0,125),
     las=2,type="n")
# Add light grey lines for all the simulations
for (run in sim.data) {
    lines(the.months, adoption.curve(run), col="lightgrey")
}
# Overlay the actual data in black
lines(the.months,adoption.curve(ckm))
@
\caption{Real (black) and simulated (grey) number of cumulative adoptions over
time.}
\label{fig:adoption-curves-real-vs-sim}
\end{figure}

\begin{figure}
<<echo=FALSE>>=
# Distribution of differences in adoption dates between neighbors
# Inputs: data frame with adoption dates, network
# Output: cross-tabulation table
adoption.association <- function(data,net) {
  # First, work out all (distinct) pairs of doctors, and make those columns
  # in a data frame
  dr.pairs <- data.frame(t(combn(x=1:nrow(data),m=2)))
  # Name the two columns
  colnames(dr.pairs) <- c("Doctor.1","Doctor.2")
  # Add a column which is the adoption date of the first doctor
  dr.pairs$Date.1 <- data$adoption_date[dr.pairs$Doctor.1]
  # Add a column which is the adoption date of the 2nd doctor IF they know
  # each other, and NA if they do not
  dr.pairs$Date.2 <- ifelse(diag(net[dr.pairs$Doctor.1,dr.pairs$Doctor.2])==1,
                            data$adoption_date[dr.pairs$Doctor.2],
                            NA)
  # Drop rows with NAs, as corresponding to doctors who don't know each other
  dr.pairs <- na.omit(dr.pairs)
  # Retun a table of absolute values of differences in dates
  return(table(abs(dr.pairs$Date.1 - dr.pairs$Date.2),useNA="ifany"))
}
@
<<echo=FALSE>>=
assoc.plot<- function(data,net,...) {
    assoc <- adoption.association(data,net)
    invisible(lines(as.numeric(names(assoc)),assoc,...))
}
plot(0,xlim=c(0,16),ylim=c(0,50),xlab="Lag in months",ylab="Frequency",type="n")
invisible(sapply(sim.data,assoc.plot,net=net,col="lightgrey"))
assoc.plot(ckm,net,col="black")
@
\caption{Association of adoption dates across neighboring doctors:
horizontal axis, lag, or difference in adoption dates, in months; vertical axis,
frequency of lag, among all pairs of distinct doctors.  Grey, simulation
runs; black, data.}
\label{fig:associated-adoptions}
\end{figure}

\begin{figure}
<<sim.bootstrap,echo=FALSE>>=
# Some runs of the simulation don't have quite enough distinct values of C
# for s() to be happy at its default setting, so use a smaller number of
# knots
  # Done after checking that this does not alter the estimate on the original
  # data
binary.curve <- function(df) {
    mdl <- gam(adopting ~ s(peer.pressure,k=6),data=df[df[,"already"]==0,],
               family="binomial")
    preds <- predict(mdl,type="response",newdata=data.frame(peer.pressure=0:30))
    return(preds)
}

plot(0,xlim=c(0,30),ylim=c(0,1), xlab="Number of contacts previously adopting",
     ylab="Adoption probability", type="n")
rug(x=jitter(df.3$peer.pressure[df.3$already==0]),side=1)
binary.curve.plot <- function(data,...) { lines(0:30,binary.curve(data),...) }
invisible(sapply(sim.runs,binary.curve.plot,col="lightgrey"))
lines(0:30,preds.3$fit,col="black")
lines(0:30, pmin(1,preds.3$fit+2*preds.3$se.fit),col="black",lty="dashed")
lines(0:30, pmax(0,preds.3$fit-2*preds.3$se.fit),col="black",lty="dashed")
@
\caption{Estimated probabilities from fitting the logistic GAM of problem 3 on
  the data (black, plus or minus two nominal standard errors, dashed, as in
  Figure \ref{fig:pred3}), and the results of fitting the same model to full
  network simulations of the model from problem 5 (grey curves).}
\label{fig:sim.bootstrap}
\end{figure}


\end{document}
