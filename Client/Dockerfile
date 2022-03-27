FROM haskell:8.10.6

WORKDIR /src

RUN cabal update 

COPY ./final-lambda.cabal /src/final-lambda.cabal
COPY ./cabal.project /src/cabal.project

RUN apt update && apt upgrade -y
RUN apt-get install freeglut3 freeglut3-dev libx11-dev libxrandr-dev libxinerama-dev libxcursor-dev libglvnd-dev libxi-dev -y

RUN cabal build --only-dependencies -j4

COPY . /src

RUN cabal install 

# ENTRYPOINT ["final-lambda"]
ENTRYPOINT ["tail", "-f", "/dev/null"]

# GL, X11, Xi, Xrandr, Xxf86vm, Xcursor, Xinerama