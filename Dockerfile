ARG LTS=latest
FROM fpco/stack-build:$LTS AS builder
ARG LTS=latest

WORKDIR /code
# precache the bulk of requiremtns
RUN stack --resolver $LTS  build hakyll

# copy files that do not effect the build much first to precommute as much as
# possible
COPY ./stack.yaml ./kyleondy-com.cabal ./
RUN stack build --dependencies-only
COPY . .
RUN make test

FROM nginx:stable
COPY --from=builder /code/_site/ /usr/share/nginx/html
