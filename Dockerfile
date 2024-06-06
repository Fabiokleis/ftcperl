FROM erlang:24-alpine AS build

RUN mkdir /app
WORKDIR /app

RUN apk add --no-cache build-base

COPY config config/
COPY apps apps/
COPY rebar.config .

RUN rebar3 release

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
     apk add --no-cache libstdc++

#COPY --from=build /app/_build/default/rel/ftcperl /rel

CMD ["/app/_build/default/rel/ftcperl/bin/ftcperl", "foreground"]
