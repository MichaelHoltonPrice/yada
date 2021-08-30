# The following two commands can be used to build the Docker image and start a
# container. The -v tag mirrors a folder on the host machine with the /data
# folder in the Docker container.
#
# docker build -t michaelholtonprice/yada .
# docker run --name yada -itv /yada_docker_data:/data michaelholtonprice/yada
#
# If desired, the following command starts a container without mirroring a
# directory on the host machine:
#
# docker run --name yada -it michaelholtonprice/yada
FROM ubuntu:20.04

# Set the following environmental variable to avoid interactively setting the
# timezone with tzdata when installing R
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y vim && \
    apt-get install -y git && \
    apt-get install -y apt-transport-https && \
    apt-get install -y software-properties-common && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
    add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/' && \
    apt-get update && \
    apt-get install -y r-base && \
    apt-get install -y libcurl4-openssl-dev && \
    apt-get install -y libssl-dev && \
    apt-get install -y libxml2-dev && \
    apt-get install -y pandoc && \
    apt-get clean

# Clone the latest version of the yada repository
RUN git clone https://github.com/MichaelHoltonPrice/yada

# Set the working directory to the code directory
WORKDIR /yada

# Install yada by running the install_yada.R script
RUN Rscript install_yada.R