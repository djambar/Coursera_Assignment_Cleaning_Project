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
    \item{\code{tbody.gyro.mean.x}}{mean of the gyroscopic signal of the body component, on the x-axis and the time domain}
    \item{\code{tbody.gyro.mean.y}}{mean of the gyroscopic signal of the body component, on the y-axis and the time domain}
    \item{\code{tbody.gyro.mean.z}}{mean of the gyroscopic signal of the body component, on the z-axis and the time domain}
    \item{\code{tbody.gyro.std.x}}{standard deviation of the gyroscopic signal of the body component, on the x-axis and the time domain}
    \item{\code{tbody.gyro.std.y}}{standard deviation of the gyroscopic signal of the body component, on the y-axis and the time domain}
    \item{\code{tbody.gyro.std.z}}{standard deviation of the gyroscopic signal of the body component, on the z-axis and the time domain}
    \item{\code{tbody.gyrojerk.mean.x}}{mean of the jerk of the gyroscopic component of the body aceleration on the x-axis and time domain}
    \item{\code{tbody.gyrojerk.mean.y}}{mean of the jerk of the gyroscopic component of the body aceleration on the y-axis and time domain}
    \item{\code{tbody.gyrojerk.mean.z}}{mean of the jerk of the gyroscopic component of the body aceleration on the z-axis and time domain}
    \item{\code{tbody.gyrojerk.std.x}}{standard deviation of the jerk of the gyroscopic component of the body aceleration on the x-axis and time domain}
    \item{\code{tbody.gyrojerk.std.y}}{standard deviation of the jerk of the gyroscopic component of the body aceleration on the y-axis and time domain}
    \item{\code{tbody.gyrojerk.std.z}}{standard deviation of the jerk of the gyroscopic component of the body aceleration on the z-axis and time domain}
    \item{\code{tbody.accmag.mean}}{mean magnitude of the acceleration of the body component, in the time domain}
    \item{\code{tbody.accmag.std}}{standard deviation of the magnitude of the acceleration of the body component, in the time domain}
    \item{\code{tgravity.accmag.mean}}{mean magnitude of the gravity acceleration, in the time domain}
    \item{\code{tgravity.accmag.std}}{standard deviation of the magnitude of the gravity acceleration, in the time domain}
    \item{\code{tbody.accjerkmag.mean}}{mean magnitude of the jerk of the body acceleration, in the time domain}
    \item{\code{tbody.accjerkmag.std}}{standard deviation of the magnitude of the jerk of the body acceleration, in the time domain}
    \item{\code{tbody.gyromag.mean}}{mean magnitude of the gyroscope signal in the time domain}
    \item{\code{tbody.gyromag.std}}{standard deviation of the magnitude of the gyroscope signal in the time domain}
    \item{\code{tbody.gyrojerkmag.mean}}{mean magnitude of the jerk of the gyroscope signal for the body component, in the time domain}
    \item{\code{tbody.gyrojerkmag.std}}{standard deviation of the  magnitude of the jerk of the gyroscope signal for the body component, in the time domain}
    \item{\code{fbody.acc.mean.x}}{from this variable to the last, we have the same description as above, but in the frequency domain (with prefixes f instead on t)}
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
For more information about this dataset contact: activityrecognition@smartlab.ws
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
