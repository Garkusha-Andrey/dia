# Controller

## How To
A short instruction how to start controller and diameter in cloudify:

Controller startup: prepare config files controllerA[B,C].config at each VM
                    start controllers together
```
# prepare config file for controller
diameter-relay-agent]> boot/config.sh -c1ip='10.10.0.55' -c2ip='10.10.0.56' -c3ip='10.10.0.57'

# start controllers at each VM. Change **-contrprio=A** filed to **B|C** in accordance with node
diameter-relay-agent/controller/ebin]> ../../boot/boot.sh -a=controller_app -netIf=eth1 -ovsIntIp=10.10.1.56 -ovsIntMask=255.255.255.0 -publicIp=10.10.2.2 -publicMask=255.255.255.0 -ovsMac=fa:16:3e:64:d2:c3 -extGwMac=fa:16:3e:08:da:fc -contrprio=A
```

Start diameters instanses
```
diameter-relay-agent/controller/ebin]> ../../boot/boot.sh -a=diameter -netIf=eth2 -rnode=controllerA@10.10.0.55 -inst=001
```

After that you can  trigger initial distribution
```
controller_app:clodify_done().
```
