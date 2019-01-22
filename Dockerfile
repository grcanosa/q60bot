FROM python:3.6-alpine3.8

LABEL author_name Gonzalo Rodriguez
LABEL author_email grcanosa@gmail.com

WORKDIR /code

COPY requirements.txt .
#RUN apk add --no-cache openssl-dev libffi-dev
RUN pip3 install --trusted-host=pypi.python.org --trusted-host=pypi.org --trusted-host=files.pythonhosted.org -r requirements.txt