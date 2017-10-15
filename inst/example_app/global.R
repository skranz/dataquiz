# courser  coursepage app
library(dataquiz)

Sys.umask("000")

quiz.dir = "/srv/quizdir"
app = dataquizApp(quiz.dir)

appReadyToRun(app)

#viewApp(app)
