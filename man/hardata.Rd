\name{hardata}
\alias{hardata}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("hardata")}
\format{
  A data frame with 180 observations on the following 81 variables. All the variables are numeric, except the activity (factor).From the third variable to the last, we have tri-axial measures  of signals. 
  \describe{
    \item{\code{activity}}{a factor with levels \code{WALKING} \code{WALKING_UPSTAIRS} \code{WALKING_DOWNSTAIRS} \code{SITTING} \code{STANDING} \code{LAYING}}
    \item{\code{subject}}{the Id of the participant}
    \item{\code{tbody.acc.mean.x}}{mean of the body acceleration, in time domain, on the x-axis}
    \item{\code{tbody.acc.mean.y}}{mean of the body acceleration, in time domain, on the y-axis}
    \item{\code{tbody.acc.mean.z}}{mean of the body acceleration, in time domain, on the y-axis}
    \item{\code{tbody.acc.std.x}}{standard deviation of the body acceleration, in time domain, on the x-axis}
    \item{\code{tbody.acc.std.y}}{standard deviation of the body acceleration, in time domain, on the y-axis}
    \item{\code{tbody.acc.std.z}}{standard deviation of the body acceleration, in time domain, on the z-axis}
    \item{\code{tgravity.acc.mean.x}}{mean of the gravitationnal component of the acceleration on the x-axis and time domain}
    \item{\code{tgravity.acc.mean.y}}{mean of the gravitationnal component of the acceleration on the y-axis and time domain}
    \item{\code{tgravity.acc.mean.z}}{mean of the gravitationnal component of the acceleration on the z-axis and time domain}
    \item{\code{tgravity.acc.std.x}}{standard deviation of the gravitationnal component of the acceleration on the x-axis and time domain}
    \item{\code{tgravity.acc.std.y}}{standard deviation of the gravitationnal component of the acceleration on the y-axis and time domain}
    \item{\code{tgravity.acc.std.z}}{tgravity.acc.std.y}}{standard deviation of the gravitationnal component of the acceleration on the z-axis and time domain}
    \item{\code{tbody.accjerk.mean.x}}{mean jerk of the acceleration of the body, in the time domain and the x-axis}
    \item{\code{tbody.accjerk.mean.y}}{mean jerk of the acceleration of the body, in the time domain and the y-axis}
    \item{\code{tbody.accjerk.mean.z}}{mean jerk of the acceleration of the body, in the time domain and the z-axis}
    \item{\code{tbody.accjerk.std.x}}{standard deviation of the jerk of the acceleration of the body, in the time domain and the x-axis}
    \item{\code{tbody.accjerk.std.y}}{standard deviation of the jerk of the acceleration of the body, in the time domain and the y-axis}
    \item{\code{tbody.accjerk.std.z}}{standard deviation of the jerk of the acceleration of the body, in the time domain and the z-axis}
    \item{\code{tbody.gyro.mean.x}}{a numeric vector}
    \item{\code{tbody.gyro.mean.y}}{a numeric vector}
    \item{\code{tbody.gyro.mean.z}}{a numeric vector}
    \item{\code{tbody.gyro.std.x}}{a numeric vector}
    \item{\code{tbody.gyro.std.y}}{a numeric vector}
    \item{\code{tbody.gyro.std.z}}{a numeric vector}
    \item{\code{tbody.gyrojerk.mean.x}}{a numeric vector}
    \item{\code{tbody.gyrojerk.mean.y}}{a numeric vector}
    \item{\code{tbody.gyrojerk.mean.z}}{a numeric vector}
    \item{\code{tbody.gyrojerk.std.x}}{a numeric vector}
    \item{\code{tbody.gyrojerk.std.y}}{a numeric vector}
    \item{\code{tbody.gyrojerk.std.z}}{a numeric vector}
    \item{\code{tbody.accmag.mean}}{a numeric vector}
    \item{\code{tbody.accmag.std}}{a numeric vector}
    \item{\code{tgravity.accmag.mean}}{a numeric vector}
    \item{\code{tgravity.accmag.std}}{a numeric vector}
    \item{\code{tbody.accjerkmag.mean}}{a numeric vector}
    \item{\code{tbody.accjerkmag.std}}{a numeric vector}
    \item{\code{tbody.gyromag.mean}}{a numeric vector}
    \item{\code{tbody.gyromag.std}}{a numeric vector}
    \item{\code{tbody.gyrojerkmag.mean}}{a numeric vector}
    \item{\code{tbody.gyrojerkmag.std}}{a numeric vector}
    \item{\code{fbody.acc.mean.x}}{same description as above, but in the frequency domain}
    \item{\code{fbody.acc.mean.y}}{a numeric vector}
    \item{\code{fbody.acc.mean.z}}{a numeric vector}
    \item{\code{fbody.acc.std.x}}{a numeric vector}
    \item{\code{fbody.acc.std.y}}{a numeric vector}
    \item{\code{fbody.acc.std.z}}{a numeric vector}
    \item{\code{fbody.acc.mean.freq.x}}{a numeric vector}
    \item{\code{fbody.acc.mean.freq.y}}{a numeric vector}
    \item{\code{fbody.acc.mean.freq.z}}{a numeric vector}
    \item{\code{fbody.accjerk.mean.x}}{a numeric vector}
    \item{\code{fbody.accjerk.mean.y}}{a numeric vector}
    \item{\code{fbody.accjerk.mean.z}}{a numeric vector}
    \item{\code{fbody.accjerk.std.x}}{a numeric vector}
    \item{\code{fbody.accjerk.std.y}}{a numeric vector}
    \item{\code{fbody.accjerk.std.z}}{a numeric vector}
    \item{\code{fbody.accjerk.mean.freq.x}}{a numeric vector}
    \item{\code{fbody.accjerk.mean.freq.y}}{a numeric vector}
    \item{\code{fbody.accjerk.mean.freq.z}}{a numeric vector}
    \item{\code{fbody.gyro.mean.x}}{a numeric vector}
    \item{\code{fbody.gyro.mean.y}}{a numeric vector}
    \item{\code{fbody.gyro.mean.z}}{a numeric vector}
    \item{\code{fbody.gyro.std.x}}{a numeric vector}
    \item{\code{fbody.gyro.std.y}}{a numeric vector}
    \item{\code{fbody.gyro.std.z}}{a numeric vector}
    \item{\code{fbody.gyro.mean.freq.x}}{a numeric vector}
    \item{\code{fbody.gyro.mean.freq.y}}{a numeric vector}
    \item{\code{fbody.gyro.mean.freq.z}}{a numeric vector}
    \item{\code{fbody.accmag.mean}}{a numeric vector}
    \item{\code{fbody.accmag.std}}{a numeric vector}
    \item{\code{fbody.accmag.mean.freq}}{a numeric vector}
    \item{\code{fbody.accjerkmag.mean}}{a numeric vector}
    \item{\code{fbody.accjerkmag.std}}{a numeric vector}
    \item{\code{fbody.accjerkmag.mean.freq}}{a numeric vector}
    \item{\code{fbody.gyromag.mean}}{a numeric vector}
    \item{\code{fbody.gyromag.std}}{a numeric vector}
    \item{\code{fbody.gyromag.mean.freq}}{a numeric vector}
    \item{\code{fbody.gyrojerkmag.mean}}{a numeric vector}
    \item{\code{fbody.gyrojerkmag.std}}{a numeric vector}
    \item{\code{fbody.gyrojerkmag.mean.freq}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}

\source{
}
\references{UCI Machine Learining Repository
\url{http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones}
}
\examples{
data(hardata)
## maybe str(hardata) ; plot(hardata) ...
}
\keyword{datasets}
