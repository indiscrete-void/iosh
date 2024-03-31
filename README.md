# IO shell
## Examples
```sh
# Shell over arbitrary socat address
socat "$1" exec:ioshd         # /usr/bin/soshd
iosh -et "socat $1 -" bash -l # /usr/bin/sosh

# Socat-shell with TLS
soshd ssl-l:2222,reuseaddr,fork,cert=/etc/soshd/id.pem,cafile=/etc/soshd/root.crt
sosh ssl:$1,cert=/etc/sosh/id.pem,cafile=/etc/sosh/root.crt

# Shell over pnet (see https://gitlab.com/indiscrete_void/pnet)
pnetd ioshd # on ffd8e654b8271c489b2d4cd236c327d4f4091f0958b31af8d6d893905a1ef6c3
iosh -t "pnet tunnel ffd8e654b8271c489b2d4cd236c327d4f4091f0958b31af8d6d893905a1ef6c3 -" sh
```
