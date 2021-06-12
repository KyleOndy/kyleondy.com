FROM nginx:stable
COPY ./result /usr/share/nginx/html
EXPOSE 80
