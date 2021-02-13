FROM ocaml/opam:latest
RUN /bin/sh -c 'sudo rm /etc/sudoers.d/opam'
ENTRYPOINT []
CMD ["bash"]
