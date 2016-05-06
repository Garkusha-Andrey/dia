# Diameter

## How to

### Deploy diameter client

Format of the deploy command is:   
```deploy([<Name>, <Ralm>, <Local IP>, <Remote IP>, <Port>])```   
    Name - name of the service   
    Realm - diameter Realm   
    Local IP - source IP of the diameter client   
    Remote IP - diameter server IP   
    Port - diameter server port   
    
Example
```
# from bash
diameter-relay-agent/dia/ebin> erl -sname c1 -s client deploy 'c1' 'ex.com' "127.0.0.1" "127.0.0.1" 3911

# from Erlang mashine
(c1@node)1> client:deploy(['c1','ex.com','127.0.0.1','127.0.0.1','3911']).
```

### Deploy diameter server

Seems similar with  deploy of the client.   
Format of the deploy command is:   
```deploy([<Name>, <Ralm>, <IP>, <Port>])```

Example
```
# from bash
diameter-relay-agent/dia/ebin>  erl -sname s1 -s server deploy 's1' 'ex.com' "127.0.0.1" 3911

# from Erlang mashine
(s1@node)1> server:deploy(['s1','ex.com','127.0.0.1','3911']).
```
