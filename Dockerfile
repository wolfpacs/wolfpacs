# Build

FROM erlang:22-alpine as builder

RUN apk --upgrade add --no-cache alpine-sdk

WORKDIR /build
COPY . /build
RUN rebar3 as prod tar

# Pack

FROM alpine:3.9

RUN apk --upgrade add --no-cache ncurses-dev zlib-dev bash strace libstdc++

ENV WOLFPACS_DIR="/wolfpacs"

COPY --from=builder /build/_build/prod/rel/wolfpacs/wolfpacs-*.tar.gz /tmp
RUN tar xvfh /tmp/wolfpacs-*.tar.gz -C /usr/local

RUN addgroup -S appgroup && adduser -S appuser -G appgroup
USER appuser

EXPOSE 11112
EXPOSE 11113
EXPOSE 8080

ENTRYPOINT ["/usr/local/bin/wolfpacs"]
CMD ["foreground"]
