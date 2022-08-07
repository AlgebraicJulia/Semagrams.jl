FROM continuumio/miniconda3

RUN conda create -n env
RUN echo "source activate env" > ~/.bashrc
ENV PATH /opt/conda/envs/env/bin:$PATH

COPY quarto.deb .
RUN dpkg -i quarto.deb
