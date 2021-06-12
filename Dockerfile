FROM nginx:stable
COPY ./_site /usr/share/nginx/html
EXPOSE 80
