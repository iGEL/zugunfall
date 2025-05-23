FROM node

ENV CLOJURE_VERSION=1.12.0.1530
ENV APP_DIR=/zugunfall

RUN apt-get update \
  && apt-get -q -y install openjdk-17-jdk curl poppler-utils

RUN curl -s https://download.clojure.org/install/linux-install-${CLOJURE_VERSION}.sh | bash

RUN mkdir -p $APP_DIR
WORKDIR $APP_DIR

COPY deps.edn package.json shadow-cljs.edn yarn.lock $APP_DIR
COPY src $APP_DIR/src

RUN cd $APP_DIR \
  && yarn install \
  && yarn build

CMD ["node", "/zugunfall/target/app.js"]
