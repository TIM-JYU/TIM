FROM ghcr.io/tim-jyu/tim:e5cadc85ffe5f004064ba75c19655f6d681d4b0a

LABEL devcontainer.metadata="{\"customizations\":{\"vscode\":{\"extensions\":[\"ms-python.python\",\"ms-python.mypy-type-checker\",\"ms-python.black-formatter\",\"Angular.ng-template\",\"esbenp.prettier-vscode\",\"dbaeumer.vscode-eslint\",\"wholroyd.jinja\"]}},\"remoteUser\":\"root\",\"shutdownAction\":\"none\"}"

ENV APT_INSTALL="DEBIAN_FRONTEND=noninteractive apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get -q install --no-install-recommends -y" \
    APT_CLEANUP="rm -rf /var/lib/apt/lists /usr/share/doc ~/.cache"

COPY pyproject.toml poetry.lock ./
RUN bash -c " \
    poetry config virtualenvs.create false && \
    poetry config virtualenvs.in-project false && \
    poetry install --only=dev && \
    ${APT_CLEANUP} && \
    rm pyproject.toml poetry.lock"

CMD [ "sleep", "forever" ]