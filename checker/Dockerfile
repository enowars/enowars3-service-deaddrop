FROM python

RUN apt-get update
RUN apt-get install -y nginx git

RUN pip3 install uwsgi

WORKDIR /checker

COPY requirements.txt .
RUN pip3 install -r requirements.txt

COPY checker.py .
COPY docker-compose.yml .
COPY Dockerfile .
COPY nginx.conf .
COPY uwsgi.ini .

#ENTRYPOINT ["/bin/bash"]
# ENTRYPOINT [ "uwsgi", "--uid", "uwsgi", \
#                "--socket", "0.0.0.0:3031", \
#                "--protocol", "uwsgi", \
#                "--ini", "/checker/uwsgi.ini" ]
ENTRYPOINT ["uwsgi", "--uid", "uwsgi", "--socket", "[::]:3031", "--protocol", "http", "--ini", "./uwsgi.ini"]