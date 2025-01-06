from gerbil/ubuntu

RUN apt-get update
RUN apt-get install --no-install-recommends --no-install-suggests -y flex bison build-essential csh libxaw7-dev
RUN apt-get install sudo
RUN sudo sudo mkdir /usr/class
WORKDIR /usr/class
RUN sudo apt-get install wget
RUN wget https://courses.edx.org/asset-v1:StanfordOnline+SOE.YCSCS1+1T2020+type@asset+block@student-dist.tar.gz
RUN sudo apt-get install tar
RUN mv asset-v1:StanfordOnline+SOE.YCSCS1+1T2020+type@asset+block@student-dist.tar.gz assets.tar.gz
RUN tar -xf assets.tar.gz
RUN sudo apt-get install -y lib32ncurses6 lib32z1
RUN mkdir workspace
CMD ["/bin/bash"]
