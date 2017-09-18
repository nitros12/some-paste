FROM ubuntu:14.04

RUN ["apt-get", "update"]
RUN ["apt-get" "-y", "install", "nginx"]

RUN ln -sf /dev/stdout /var/log/nginx/access.log
run ln -sf /dev/stderr /var/log/nginx/error.log

VOLUME ["/var/cache/nginx"]
EXPOSE 80 443

ADD ./site.conf /etc/nginx/sites-available/some-paste
RUN ["ln", "-s", "/etc/nginx/sites-availabe/some-paste", "/etc/nginx/sites-enabled/some-paste"]
RUN ["rm", "-rf", "/etc/nginx/sites-available/default"]

RUN ["groupadd", "webserver"]
RUN ["useradd", "webserver", "-s", "/bin/bash", "-m", "-g", "webserver", "-G", "webserver"]
ENV HOME /home/webserver
WORKDIR /home/webserver

RUN ["chown", "-R", "webserver:webserver", "/home/webserver"]

CMD ["nginx", "-g", "daemon off;"]