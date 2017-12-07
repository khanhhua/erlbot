FROM erlang:19.3.6

LABEL maintainer="khanh.hua@axa.com.sg"
# Set exposed ports
EXPOSE 8080
ENV PORT=8080

ADD . /erlbot

WORKDIR /erlbot/

#CMD ["bash"]
RUN rebar3 release
CMD ["_build/default/rel/erlbot-alpha/bin/erlbot-alpha", "foreground"]
