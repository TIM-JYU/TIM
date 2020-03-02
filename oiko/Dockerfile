# Version of Python that this image uses
FROM python:3.6.2-stretch

# Working directory inside the image
WORKDIR /code

# Flasks environment variable
ENV FLASK_APP app.py

# Flasks environment variable
ENV FLASK_RUN_HOST 0.0.0.0

# Install Voikko packages 
RUN DEBIAN_FRONTEND=noninteractive \
	&& apt-get update \
	&& apt-get install -y voikko-fi python-libvoikko

# Copy libvoikko to working directory
RUN cp /usr/lib/python3/dist-packages/libvoikko.py /code/libvoikko.py

# Copy requirements from host to container
COPY requirements.txt requirements.txt

# Install everything that is defined in requirements.txt
RUN pip install -r requirements.txt

# RUN python3 -m pip install gunicorn
# RUN python3 -m pip install gevent

# Copy this directory to workdir of image
COPY . .

# Set the default command for the container to flask run
# CMD ["flask", "run"]
CMD python3 launch.py