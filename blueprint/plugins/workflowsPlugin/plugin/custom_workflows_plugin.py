########
# Copyright (c) 2014 GigaSpaces Technologies Ltd. All rights reserved
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
#    * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    * See the License for the specific language governing permissions and
#    * limitations under the License.

import paramiko
import urllib2
import json
import time
from cloudify import constants, utils
from cloudify.decorators import workflow
from cloudify.plugins import lifecycle


@workflow
def custom_install(ctx, **kwargs):
    """Default install workflow"""

    lifecycle.install_node_instances(
        graph=ctx.graph_mode(),
        node_instances=set(ctx.node_instances))
    ips=get_controllers_ips(ctx)
    cloudify_done(ctx,ips)


@workflow
def custom_auto_heal_reinstall_node_subgraph(
        ctx,
        node_instance_id,
        diagnose_value='Not provided',
        **kwargs):
    """Reinstalls the whole subgraph of the system topology
    The subgraph consists of all the nodes that are hosted in the
    failing node's compute and the compute itself.
    Additionally it unlinks and establishes appropriate relationships
    :param ctx: cloudify context
    :param node_id: failing node's id
    :param diagnose_value: diagnosed reason of failure
    """

    ctx.logger.info("Starting 'heal' workflow on {0}, Diagnosis: {1}"
                    .format(node_instance_id, diagnose_value))
    failing_node = ctx.get_node_instance(node_instance_id)
    failing_node_host = ctx.get_node_instance(
        failing_node._node_instance.host_id
    )
    subgraph_node_instances = failing_node_host.get_contained_subgraph()
    intact_nodes = set(ctx.node_instances) - subgraph_node_instances
    graph = ctx.graph_mode()
    lifecycle.reinstall_node_instances(
        graph=graph,
        node_instances=subgraph_node_instances,
        intact_nodes=intact_nodes)
    ips=get_controllers_ips(ctx)
    cloudify_done(ctx,ips)

    
@workflow
def custom_scale(ctx, node_id, delta, scale_compute, **kwargs):
    graph = ctx.graph_mode()
    node = ctx.get_node(node_id)
    if not node:
        raise ValueError("Node {0} doesn't exist".format(node_id))
    if delta == 0:
        ctx.logger.info('delta parameter is 0, so no scaling will take place.')
        return
    host_node = node.host_node
    scaled_node = host_node if (scale_compute and host_node) else node
    curr_num_instances = scaled_node.number_of_instances
    planned_num_instances = curr_num_instances + delta
    if planned_num_instances < 0:
        raise ValueError('Provided delta: {0} is illegal. current number of '
                         'instances of node {1} is {2}'
                         .format(delta, node_id, curr_num_instances))

    modification = ctx.deployment.start_modification({
        scaled_node.id: {
            'instances': planned_num_instances

            # These following parameters are not exposed at the moment,
            # but should be used to control which node instances get scaled in
            # (when scaling in).
            # They are mentioned here, because currently, the modification API
            # is not very documented.
            # Special care should be taken because if `scale_compute == True`
            # (which is the default), then these ids should be the compute node
            # instance ids which are not necessarily instances of the node
            # specified by `node_id`.

            # Node instances denoted by these instance ids should be *kept* if
            # possible.
            # 'removed_ids_exclude_hint': [],

            # Node instances denoted by these instance ids should be *removed*
            # if possible.
            # 'removed_ids_include_hint': []
        }
    })
    try:
        ctx.logger.info('Deployment modification started. '
                        '[modification_id={0}]'.format(modification.id))
        if delta > 0:
            added_and_related = set(modification.added.node_instances)
            added = set(i for i in added_and_related
                        if i.modification == 'added')
            related = added_and_related - added
            try:
                lifecycle.install_node_instances(
                    graph=graph,
                    node_instances=added,
                    intact_nodes=related)
            except:
                ctx.logger.error('Scale out failed, scaling back in.')
                for task in graph.tasks_iter():
                    graph.remove_task(task)
                lifecycle.uninstall_node_instances(
                    graph=graph,
                    node_instances=added,
                    intact_nodes=related)
                raise
        else:
            removed_and_related = set(modification.removed.node_instances)
            removed = set(i for i in removed_and_related
                          if i.modification == 'removed')
            related = removed_and_related - removed
            lifecycle.uninstall_node_instances(
                graph=graph,
                node_instances=removed,
                intact_nodes=related)
    except:
        ctx.logger.warn('Rolling back deployment modification. '
                        '[modification_id={0}]'.format(modification.id))
        try:
            modification.rollback()
        except:
            ctx.logger.warn('Deployment modification rollback failed. The '
                            'deployment model is most likely in some corrupted'
                            ' state.'
                            '[modification_id={0}]'.format(modification.id))
            raise
        raise
    else:
        try:
            modification.finish()
        except:
            ctx.logger.warn('Deployment modification finish failed. The '
                            'deployment model is most likely in some corrupted'
                            ' state.'
                            '[modification_id={0}]'.format(modification.id))
            raise
    ips=get_controllers_ips(ctx)
    cloudify_done(ctx,ips)


def _filter_node_instances(ctx, node_ids, node_instance_ids, type_names):
    filtered_node_instances = []
    for node in ctx.nodes:
        if node_ids and node.id not in node_ids:
            continue
        if type_names and not next((type_name for type_name in type_names if
                                    type_name in node.type_hierarchy), None):
            continue

        for instance in node.instances:
            if node_instance_ids and instance.id not in node_instance_ids:
                continue
            filtered_node_instances.append(instance)
    return filtered_node_instances


def _get_all_host_instances(ctx):
    node_instances = set()
    for node_instance in ctx.node_instances:
        if lifecycle.is_host_node(node_instance):
            node_instances.add(node_instance)
    return node_instances

###########################################################################################    
## Additional functions for executing of the cloudify_done erlang function on controller ##    
########################################################################################### 
## Get token for providinf access to the Openstack API
## Return value: token for requests to the API
def get_auth_token():
    url='http://192.168.203.210:5000/v3/auth/tokens'
    file=open('/home/centos/token_cloudify.json','r')
    data=file.read().replace('\n','').replace(' ','')
    file.close()
    headers={'Content-type' : 'application/json'}
    req=urllib2.Request(url,data,headers)
    response=urllib2.urlopen(req)
    token=response.info().getheader('X-Subject-Token')
    return token

## Find ip address of the controller instance from cloudify-management-network via Openstack API
## Return value: List of the controllers ips
def get_controllers_ips(ctx):
    target_nodes=['controller1','controller2','controller3']
    target_nodes_ips=[]
    for node in ctx.nodes:
        ctx.logger.info("node={0}".format(node.id))
        try:
            check=target_nodes.index(node.id)
        except ValueError:
            ctx.logger.info("node={0} not in target_nodes list".format(node.id))
        else:
            ctx.logger.info("node={0} in target_nodes list".format(node.id))
            correct_node_name=node.id.replace('c', 'C') + "_VM"
            token=get_auth_token()
            url_for_ids='http://192.168.203.210:8774/v2.1/c5cb11f2e7604f30bae0b61cec0086f7/servers'
            header={'X-Auth-Token' : '%s' %token }
            req_for_ids=urllib2.Request(url_for_ids,None,header)
            resp_for_ids=urllib2.urlopen(req_for_ids)
            ids_json=json.loads(resp_for_ids.read())
            for obj in ids_json['servers']:
                if obj['name'] == correct_node_name:
                    id=obj['id'].encode("ascii","ignore")
            
            ctx.logger.info("id={0}".format(id))
            
            url_for_ip='http://192.168.203.210:8774/v2.1/c5cb11f2e7604f30bae0b61cec0086f7/servers/%s' %id
            req_for_ip=urllib2.Request(url_for_ip,None,header)
            resp_for_ip=urllib2.urlopen(req_for_ip)
            ip_json=json.loads(resp_for_ip.read())
            ip=ip_json['server']['addresses']['cloudify-management-network'][0]['addr'].encode("ascii","ignore")
            target_nodes_ips.append(ip)
            ctx.logger.info("ip={0}".format(ip))
    ctx.logger.info("target_nodes_ips={0}".format(target_nodes_ips))
    return target_nodes_ips


## Connect to the target controller and execute erlang function via SSH   
def cloudify_done(ctx, ips):
    for ip in ips:
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        try:
            client.connect(hostname=ip, username='nfvpilot', password='nfvpilot')
        except Exception, e:
            ctx.logger.info("Connection error: {0}".format(str(e)))
        else:
            ctx.logger.info("Connection to the {0} done".format(ip))
            stdin, stdout, stderr = client.exec_command('ps -ef | grep -v grep | grep -oh \'controller[A-C]@10.10.0.5[5-7]\'')
            node_name=stdout.read()
            cmd='erl_call -c \'ABCD\' -a \'controller_app clodify_done\' -n {0}'.format(node_name)
            stdin, stdout, stderr = client.exec_command(cmd)
            ctx.logger.info("Result of cmd {0}: {1}".format(cmd,stdout.read()))
        break

## Function for execute stop ODL on controller vi SSH
## Not used in last version        
def stop_odl(ctx, ips):
    for ip in ips:
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        try:
            client.connect(hostname=ip, username='nfvpilot', password='nfvpilot')
        except Exception, e:
            ctx.logger.info("Connection error: {0}".format(str(e)))
        else:
            ctx.logger.info("Connection to the {0} done".format(ip))
            cmd_restart_odl='/home/nfvpilot/opendaylight/distribution/distributions/karaf/target/assembly/bin/stop'
            time.sleep(60)
            stdin, stdout, stderr = client.exec_command(cmd_restart_odl)
            ctx.logger.info("Result of cmd {0}: {1}".format(cmd_restart_odl,stdout.read()))
        break