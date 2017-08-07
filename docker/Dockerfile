FROM nginx:stable-alpine

COPY site.tar.gz /tmp/
RUN tar -xzf /tmp/site.tar.gz -C /usr/share/nginx/html \
    && rm /tmp/site.tar.gz

