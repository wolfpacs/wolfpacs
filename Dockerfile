FROM erlang:22-alpine as builder

WORKDIR /build
COPY . /build
RUN rebar3 as prod tar

FROM alpine:3.9

RUN apk --upgrade add --no-cache ncurses-dev zlib-dev bash strace

ENV WOLFPACS_DIR="/wolfpacs"

COPY --from=builder /build/_build/prod/rel/wolfpacs/wolfpacs-0.0.1.tar.gz /tmp
RUN tar xvfh /tmp/wolfpacs-0.0.1.tar.gz -C /usr/local

EXPOSE 11112

ENTRYPOINT ["/usr/local/bin/wolfpacs"]
CMD ["foreground"]
