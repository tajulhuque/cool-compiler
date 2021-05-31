from gerbil/scheme

RUN apt-get update
RUN apt-get install --no-install-recommends --no-install-suggests -y flex bison build-essential csh libxaw7-dev
RUN apt-get install sudo
RUN sudo sudo mkdir /usr/class
# RUN sudo chown USER /usr/class
WORKDIR /usr/class
RUN sudo apt-get install wget
RUN wget https://courses.edx.org/asset-v1:StanfordOnline+SOE.YCSCS1+1T2020+type@asset+block@student-dist.tar.gz
RUN sudo apt-get install tar
RUN mv asset-v1:StanfordOnline+SOE.YCSCS1+1T2020+type@asset+block@student-dist.tar.gz assets.tar.gz
RUN tar -xf assets.tar.gz
#RUN export PATH="/usr/class/bin:$PATH"
RUN sudo apt-get install -y emacs
# WORKDIR /root/.emacs.d/
# RUN mkdir lisp
# WORKDIR /root/.emacs.d/lisp/
# RUN wget https://raw.githubusercontent.com/vyzo/gerbil/master/etc/gerbil-mode.el
# RUN wget https://raw.githubusercontent.com/thunknyc/emacs-treadmill/master/treadmill.el
# RUN gxpkg install github.com/thunknyc/gerbil-treadmill
RUN sudo apt-get install -y lib32ncurses5 lib32z1
WORKDIR /root/

# Todo: use Copy  command to copy in an entire .emacs instead of building here

# RUN echo "(add-to-list 'load-path \"~/.emacs.d/lisp\")" >> .emacs
# RUN echo "(autoload 'gerbil-mode \"gerbil-mode\" \"Gerbil editing mode.\" t)" >> .emacs
# RUN echo "(require 'treadmill) ; will not be required with MELPA package" >> .emacs
# RUN echo "(add-hook 'gerbil-mode-hook #'treadmill-gerbil-mode)"
CMD ["/bin/bash"]
