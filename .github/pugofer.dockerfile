FROM ubuntu:22.04
WORKDIR /

# Install wget and unzip and libedit2 (required by pug)
RUN apt-get update && apt-get install -y wget unzip libedit2

# Download pug artifact from github and unzip it in /pugofer folder
RUN mkdir pugofer && \
    cd pugofer && \
    wget https://github.com/saukap/gofer/releases/latest/download/pug_linux.zip && \
    unzip pug_linux.zip && \
    chmod +x gofer

RUN useradd -ms /bin/bash student
USER student
WORKDIR /home/student

# append the pugofer folder to the PATH
ENV PATH="/pugofer:${PATH}"
# Set env PUGOFER to the default prelude
ENV PUGOFER=/pugofer/pusimple.pre

# run bash as entry point
ENTRYPOINT [ "/bin/bash" ]
