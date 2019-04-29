## Documentation & Design

[Google Doc Link](https://docs.google.com/document/d/1yY7frHaYDF8IoTSd7z3gOMtoapwcGe-EOquH1Yg8KB8/edit?usp=sharing)

[State Machine Diagram](https://github.gatech.edu/vtang7/CS2340Sp19Team15/blob/master/ScalaGameState.pdf)

## How to run locally

Start Risk:

```bash
sbt run
```

And open [http://localhost:9000/](http://localhost:9000/)

## How to open server to internet (multi-player & mobile app)

First start Risk locally, then start ssh from a separate terminal/command prompt instance:

```bash
ssh -R scala-risk.serveo.net:80:localhost:9000 scala-risk@serveo.net
```

And open [https://scala-risk.serveo.net](https://scala-risk.serveo.net)

While running: press g to open GUI, q to quit GUI, and Ctrl + C to close connection

## Credits
Play Framework - Forms Skeleton:
Originally written by Chris Birchall and the Guardian Team: <https://github.com/cb372/play-forms-tutorial>.
