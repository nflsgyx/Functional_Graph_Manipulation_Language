# Based on 18.04 LTS
FROM columbiasedwards/plt

RUN opam install merlin ocp-indent -y
RUN eval `opam config env`
WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]