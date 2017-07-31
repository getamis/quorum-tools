FROM haskell:8.0.2
MAINTAINER Edwin Hsieh <edwin@am.is>

ENV HOME /root
ENV GOLANG_GITHUB $HOME/work/src/github.com
ENV JP_REPO_PATH $GOLANG_GITHUB/jpmorganchase
ENV JP_QT_PATH $JP_REPO_PATH/quorum-tools
ENV ETH_PATH $GOLANG_GITHUB/ethereum
ENV ETH_GO_PATH $ETH_PATH/go-ethereum
ENV DOWNLOAD_PATH $HOME/download
ENV STACK_BIN_PATH $HOME/.local/bin

RUN mkdir -p $JP_QT_PATH
COPY . $JP_QT_PATH
WORKDIR $JP_QT_PATH

RUN apt-get update
RUN apt-get install -y openssh-client
RUN ssh-keygen -q -t rsa -N '' -f /id_rsa

RUN apt-get install -y git

RUN mkdir -p $DOWNLOAD_PATH && \
    cd $DOWNLOAD_PATH && \
    apt-get install -y curl && \
    curl -O https://storage.googleapis.com/golang/go1.7.3.linux-amd64.tar.gz && \
    tar xvf go1.7.3.linux-amd64.tar.gz && \
    chown -R root:root ./go && \
    mv go /usr/local && \
    echo "export GOPATH=$HOME/work" >> $HOME/.bashrc && \
    echo "export PATH=$PATH:/usr/local/go/bin:$GOPATH/bin" >> $HOME/.bashrc && \
    /bin/bash -c "source ~/.bashrc" && \
    mkdir -p $ETH_PATH && \
    cd $ETH_PATH && \
    git clone https://github.com/jpmorganchase/quorum.git -b dynamic-raft-membership --single-branch ./go-ethereum

RUN /bin/bash -c "source ~/.bashrc && cd ~/work/src/github.com/ethereum/go-ethereum/ && go run build/ci.go install ./cmd/geth" && \
    mkdir -p $STACK_BIN_PATH && \
    cp $ETH_GO_PATH/build/bin/geth $STACK_BIN_PATH

RUN apt-get install -y build-essential libdb-dev && \
    apt-get install -y software-properties-common
RUN echo "deb http://ppa.launchpad.net/chris-lea/libsodium/ubuntu trusty main" >> /etc/apt/sources.list;
RUN echo "deb-src http://ppa.launchpad.net/chris-lea/libsodium/ubuntu trusty main" >> /etc/apt/sources.list;
RUN apt-get update
RUN apt-get --yes --force-yes install libsodium-dev
RUN apt-get install -y lsof
RUN apt-get install -y iptables

RUN mkdir -p $JP_QT_PATH/.stack-work/downloaded && \
    cd $JP_QT_PATH/.stack-work/downloaded && \
    git clone https://github.com/joelburget/symmetric-properties.git ./3H__tzmzKbP- && \
    cd ../.. && \
    stack setup && \
    stack build