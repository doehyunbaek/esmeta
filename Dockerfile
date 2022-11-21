FROM sbtscala/scala-sbt:eclipse-temurin-17.0.4_1.8.0_3.2.1
ENV ESMETA_HOME=/app/esmeta
WORKDIR /app
# download esmeta
RUN mkdir -p "${ESMETA_HOME}" \
    && cd "${ESMETA_HOME}" \
    && git init \
    && git remote add origin https://github.com/es-meta/esmeta.git \
    # v0.1.0-rc5
    && git fetch --depth 1 origin 568abcfc85534c661bba3576815badffe185d334 \ 
    && git checkout FETCH_HEAD \
    && git submodule update --init --depth 1
# build esmeta
RUN cd "${ESMETA_HOME}" \ 
    && sbt assembly \
    && chmod +x /app/esmeta/bin/esmeta \
    && rm -rf /app/esmeta/target \
    && rm -rf /home/sbtuser/
# make entrypoint script
RUN echo '#!/bin/bash\n\
/app/esmeta/bin/esmeta "$@"\n' > /app/entrypoint.sh \
    && chmod +x /app/entrypoint.sh
ENTRYPOINT [ "/app/entrypoint.sh"]
